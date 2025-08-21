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
#'   influence scores. Default -24 was chosen so all well-connected neurons have 
#'   adjusted_influence >= 0.
#' @param signif Number of significant figures for output values. 
#'
#' @return Data frame with adjusted influence columns:
#' \describe{
#'   \item{adjusted_influence}{log(summed_influence) + const - basic adjusted influence}
#'   \item{adjusted_influence_norm_by_targets}{Normalized by number of target neurons: log(summed_influence/n_targets) + const}
#'   \item{adjusted_influence_norm_by_sources_and_targets}{Normalized by both source and target counts: log(summed_influence/(n_sources * n_targets)) + const}
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
                             const = -24,
                             signif = 6){
  inf.threshold <- exp(const)
  if(!"target"%in%colnames(influence_df)){
    influence_df$target <- influence_df$id
    orig.target = FALSE
  }else{
    orig.target = TRUE
  }
  orig.influence = FALSE
  if(!"influence_original"%in%colnames(influence_df)){
    if("Influence_score_(unsigned)"%in%colnames(influence_df) & "Influence_score_(signed)"%in%colnames(influence_df)){
      stop("both Influence_score_(unsigned) and Influence_score_(signed) given, assign one to influence_original")
    }else if("Influence_score_(unsigned)"%in%colnames(influence_df)){
      influence_df$influence_original <- influence_df$`Influence_score_(unsigned)`
    }else if("Influence_score_(signed)"%in%colnames(influence_df)){
      influence_df$influence_original <- influence_df$`Influence_score_(signed)`
    }else{
      stop("Please provide influence_original, Influence_score_(unsigned) or Influence_score_(signed)")
    }
  }else{
    orig.influence = TRUE
  }
  orig.seed = TRUE
  if(!"seed"%in%colnames(influence_df)){
    orig.seed = FALSE
    influence_df$seed <- "1"
  }
  influence_df <- influence_df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$seed) %>%
    dplyr::mutate(no_sources = sum(.data$is_seed, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$target) %>%
    dplyr::mutate(no_targets = length(unique(.data$id))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$target, .data$seed) %>%
    dplyr::mutate(influence_summed = sum(.data$influence_original,na.rm = TRUE),
                  influence_summed = ifelse(.data$influence_summed<inf.threshold,inf.threshold,.data$influence_summed),
                  adjusted_influence_norm_by_sources_and_targets = .data$influence_summed/(.data$no_sources*.data$no_targets),
                  adjusted_influence_norm_by_sources_and_targets = ifelse(.data$adjusted_influence_norm_by_sources_and_targets<inf.threshold,inf.threshold,.data$adjusted_influence_norm_by_sources_and_targets),
                  #no_synapses = sum(.data$pre_count,na.rm=TRUE),
                  #influence_syn_norm =  sum(.data$influence_per_synapse,na.rm = TRUE)/(.data$no_targets*.data$total_synapses),
                  #influence_syn_norm = ifelse(.data$influence_syn_norm<inf.threshold,inf.threshold,.data$influence_syn_norm),
                  #influence_syn_norm = sum(.data$influence_syn_norm,na.rm = TRUE),
                  #influence_syn_norm_log = log(.data$influence_syn_norm),
                  adjusted_influence = log(.data$influence_summed),
                  adjusted_influence_norm_by_sources_and_targets = log(.data$adjusted_influence_norm_by_sources_and_targets),
                  adjusted_influence_norm_by_targets = log((.data$influence_summed/.data$no_targets))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(#influence_syn_norm_log = .data$influence_syn_norm_log-const,
      adjusted_influence = .data$adjusted_influence-const,
      adjusted_influence_norm_by_sources_and_targets = .data$adjusted_influence_norm_by_sources_and_targets-const,
      adjusted_influence_norm_by_targets = .data$adjusted_influence_norm_by_targets-const) %>%
    dplyr::group_by(.data$seed) %>%
    dplyr::mutate(adjusted_influence = ifelse(is.na(.data$adjusted_influence),0,.data$adjusted_influence),
                  adjusted_influence_norm_by_targets = ifelse(is.na(.data$adjusted_influence_norm_by_targets),0,.data$adjusted_influence_norm_by_targets),
                  adjusted_influence_norm_by_sources_and_targets = ifelse(is.na(.data$adjusted_influence_norm_by_sources_and_targets),0,.data$adjusted_influence_norm_by_sources_and_targets)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$target, .data$seed, .keep_all = TRUE) %>%
    dplyr::mutate(adjusted_influence = signif(.data$adjusted_influence,signif),
                  adjusted_influence_norm_by_targets = signif(.data$adjusted_influence_norm_by_targets,signif),
                  adjusted_influence_norm_by_sources_and_targets = signif(.data$adjusted_influence_norm_by_sources_and_targets,signif)
    ) %>%
    dplyr::distinct(.data$target,
                    .data$seed, 
                    .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(adjusted_influence = signif(.data$adjusted_influence,signif),
                  adjusted_influence_norm_by_targets = signif(.data$adjusted_influence_norm_by_targets,signif),
                  # influence_syn_norm = signif(.data$influence_syn_norm,signif),
                  # influence_syn_norm_log = signif(.data$influence_syn_norm_log,signif),
                  adjusted_influence_norm_by_sources_and_targets = signif(.data$adjusted_influence_norm_by_sources_and_targets,signif)
    ) %>%
    dplyr::select(-"influence_summed",
                  -"no_targets",
                  -"no_sources")
  if(!orig.target){
    influence_df$target <- NULL
  }
  if(!orig.influence){
    influence_df$influence_original <- NULL
  }
  if(!orig.seed){
    influence_df$seed <- NULL
  }
  influence_df
}