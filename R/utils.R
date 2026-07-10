#' Calculate adjusted influence scores
#'
#' Computes adjusted influence scores by taking the natural logarithm of steady-state
#' neural activity and adding a constant to bring values into the non-negative range.
#' The adjusted influence is defined as: adjusted_influence = log(r) + c, where r is
#' the steady-state response and c is a constant. Poorly connected neurons with
#' log(r) << c are set to 0.
#'
#' When a 'target' column is present, influence scores are averaged within each target
#' group, allowing analysis of influence on cell types or anatomical regions rather
#' than individual neurons.
#'
#' @param influence_df Data frame, as returned by `calculate_influence`. If a 'target'
#'   column is present, influence scores will be grouped and averaged by target.
#' @param const Constant value added to log(influence) to ensure non-negative adjusted
#'   influence scores. Should be set to -log(minimum_accepted_influence) where
#'   minimum_accepted_influence is the smallest influence value considered meaningful.
#'   Default 24 corresponds to minimum_accepted_influence = exp(-24) ~ 3.78e-11.
#' @param signif Number of significant figures for output values.
#' @param minmax Logical. If TRUE, add min-max normalised columns by target and by seed.
#'   Default FALSE for backwards compatibility.
#'
#' @return Data frame with adjusted influence columns:
#' \describe{
#'   \item{adjusted_influence}{log(summed_influence) + const - basic adjusted influence}
#'   \item{adjusted_influence_norm_by_targets}{Normalized by number of target neurons: log(summed_influence/n_targets) + const}
#'   \item{adjusted_influence_norm_by_sources_and_targets}{Normalized by both source and target counts: log(summed_influence/(n_sources * n_targets)) + const}
#'   \item{adjusted_influence_minmax_by_target}{(if minmax=TRUE) Min-max normalised adjusted_influence within each target group}
#'   \item{adjusted_influence_minmax_by_seed}{(if minmax=TRUE) Min-max normalised adjusted_influence within each seed group}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic adjusted influence calculation
#' ic <- influence_calculator_r(edgelist_simple = edges.table, meta = meta)
#' influence_scores <- ic$calculate_influence(seed_ids)
#' adjusted_scores <- adjust_influence(influence_scores)
#' head(adjusted_scores)
#'
#' # Determine const based on your minimum meaningful influence
#' min_meaningful_influence <- 1e-10  # Set your threshold
#' custom_const <- -log(min_meaningful_influence)  # const = 23.03
#' adjusted_scores_custom <- adjust_influence(influence_scores, const = custom_const)
#'
#' # Group targets by cell type and calculate averaged influence
#' # This averages influence within each target group
#' influence_by_celltype <- influence_scores %>%
#'   dplyr::left_join(meta %>%
#'                      dplyr::select(root_id, target = cell_type),
#'                    by = c("id" = "root_id")) %>%
#'   adjust_influence()
#' head(influence_by_celltype)
#'
#' # Group targets by brain region
#' influence_by_region <- influence_scores %>%
#'   dplyr::left_join(meta %>%
#'                      dplyr::select(root_id, target = brain_region),
#'                    by = c("id" = "root_id")) %>%
#'   adjust_influence()
#' }
adjust_influence <- function(influence_df,
                             const = 24,
                             signif = 6,
                             minmax = FALSE){
  # Track which columns we added (to clean up later)
  added_target <- FALSE
  added_influence_original <- FALSE
  added_seed <- FALSE

  # Ensure required columns exist
  if (!"target" %in% colnames(influence_df)) {
    influence_df$target <- influence_df$id
    added_target <- TRUE
  }
  if (!"influence_original" %in% colnames(influence_df)) {
    if ("Influence_score_(unsigned)" %in% colnames(influence_df) &
        "Influence_score_(signed)" %in% colnames(influence_df)) {
      stop("both Influence_score_(unsigned) and Influence_score_(signed) given, assign one to influence_original")
    } else if ("Influence_score_(unsigned)" %in% colnames(influence_df)) {
      influence_df$influence_original <- influence_df[["Influence_score_(unsigned)"]]
    } else if ("Influence_score_(signed)" %in% colnames(influence_df)) {
      influence_df$influence_original <- influence_df[["Influence_score_(signed)"]]
    } else {
      stop("Please provide influence_original, Influence_score_(unsigned) or Influence_score_(signed)")
    }
    added_influence_original <- TRUE
  }
  if (!"seed" %in% colnames(influence_df)) {
    influence_df$seed <- "1"
    added_seed <- TRUE
  }

  # Convert to data.table for fast grouped operations
  dt <- data.table::as.data.table(influence_df)

  # Count sources per seed (number of seed neurons)
  dt[, no_sources := sum(is_seed, na.rm = TRUE), by = seed]
  dt[no_sources == 0L, no_sources := 1L]

  # Count unique target neurons per target group
  dt[, no_targets := data.table::uniqueN(id), by = target]

  # Aggregate: sum influence per (target, seed), deduplicate. The sum preserves
  # sign (so an inhibitory-dominated path returns a negative summed influence).
  dt[, influence_summed := sum(influence_original, na.rm = TRUE), by = .(target, seed)]
  dt <- unique(dt, by = c("target", "seed"), fromLast = FALSE)

  # Sign-preserving log transform: sign(x) * (log(max(|x|, exp(-const))) + const).
  # Floor magnitudes below exp(-const) to zero in either sign -- junk-node cutoff.
  .signed_adjust <- function(values, const_local) {
    floor_val <- exp(-const_local)
    mag <- pmax(abs(values), floor_val)
    out <- sign(values) * (log(mag) + const_local)
    out[abs(values) < floor_val] <- 0
    out
  }

  dt[, `:=`(
    adjusted_influence = .signed_adjust(influence_summed, const),
    adjusted_influence_norm_by_targets =
      .signed_adjust(influence_summed / no_targets, const),
    adjusted_influence_norm_by_sources_and_targets =
      .signed_adjust(influence_summed / (no_sources * no_targets), const)
  )]

  # Replace NA with 0
  for (col in c("adjusted_influence", "adjusted_influence_norm_by_targets",
                "adjusted_influence_norm_by_sources_and_targets")) {
    data.table::set(dt, which(is.na(dt[[col]])), col, 0)
  }

  # Optional min-max normalisation
  if (minmax) {
    dt[, adjusted_influence_minmax_by_target := {
      mn <- min(adjusted_influence, na.rm = TRUE)
      mx <- max(adjusted_influence, na.rm = TRUE)
      if (mx == mn) rep(0, .N) else (adjusted_influence - mn) / (mx - mn)
    }, by = target]

    dt[, adjusted_influence_minmax_by_seed := {
      mn <- min(adjusted_influence, na.rm = TRUE)
      mx <- max(adjusted_influence, na.rm = TRUE)
      if (mx == mn) rep(0, .N) else (adjusted_influence - mn) / (mx - mn)
    }, by = seed]
  }

  # Round to significant figures
  for (col in c("adjusted_influence", "adjusted_influence_norm_by_targets",
                "adjusted_influence_norm_by_sources_and_targets")) {
    data.table::set(dt, j = col, value = signif(dt[[col]], signif))
  }
  if (minmax) {
    for (col in c("adjusted_influence_minmax_by_target", "adjusted_influence_minmax_by_seed")) {
      data.table::set(dt, j = col, value = signif(dt[[col]], signif))
    }
  }

  # Clean up intermediate columns
  dt[, c("influence_summed", "no_targets", "no_sources") := NULL]

  # Remove columns we added if they weren't in the original
  if (added_target) dt[, target := NULL]
  if (added_influence_original) dt[, influence_original := NULL]
  if (added_seed) dt[, seed := NULL]

  # Convert back to tibble for backwards compatibility
  tibble::as_tibble(dt)
}
