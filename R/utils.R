# function
calculate_influence_norms <- function(influence.df,
                                      const = -24){
  inf.threshold <- exp(const)
  if(!"target"%in%colnames(influence.df)){
    influence.df$target <- influence.df$id
    orig.target = FALSE
  }else{
    orig.target = TRUE
  }
  if(!"influence_syn_original"%in%colnames(influence.df)){
    influence.df$influence_syn_norm <- 1
  }
  if(!"influence_original"%in%colnames(influence.df)){
    influence.df$influence_original <- influence.df$influence
  }
  if(!"influence_norm_original"%in%colnames(influence.df)){
    influence.df$influence_norm_original <- influence.df$influence_norm
  }
  influence.df <- influence.df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_seeds = .data$influence_original/.data$influence_norm_original,
                  no_seeds = ifelse(is.na(.data$no_seeds),1,.data$no_seeds),
                  no_synapses = .data$influence_original/.data$influence_syn_norm) %>%
    dplyr::group_by(.data$target) %>%
    dplyr::mutate(no_targets = length(unique(.data$id))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      no_seeds = as.numeric(.data$no_seeds),
      no_targets = as.numeric(.data$no_targets)
    ) %>%
    dplyr::group_by(.data$seed) %>%
    dplyr::mutate(influence_per_seed = .data$influence_original*.data$no_seeds,
                  influence_per_synapse = .data$influence_original*.data$no_synapses) %>%
    dplyr::group_by(.data$target, .data$seed) %>%
    dplyr::mutate(influence = sum(.data$influence_original,na.rm = TRUE),
                  total_seeds = sum(.data$no_seeds,na.rm=TRUE),
                  total_synapses = sum(.data$no_synapses,na.rm=TRUE),
                  influence_norm = sum(.data$influence_per_seed,na.rm = TRUE)/(.data$total_seeds*.data$no_targets),
                  influence = ifelse(.data$influence<inf.threshold,inf.threshold,.data$influence),
                  influence_norm = ifelse(.data$influence_norm<inf.threshold,inf.threshold,.data$influence_norm),
                  influence_norm = sum(.data$influence_norm,na.rm = TRUE),
                  influence_syn_norm =  sum(.data$influence_per_synapse,na.rm = TRUE)/(.data$no_targets*.data$total_synapses),
                  influence_syn_norm = ifelse(.data$influence_syn_norm<inf.threshold,inf.threshold,.data$influence_syn_norm),
                  influence_syn_norm = sum(.data$influence_syn_norm,na.rm = TRUE),
                  influence_norm_log = log(.data$influence_norm),
                  influence_log = log((.data$influence/.data$no_targets)),
                  influence_syn_norm_log = log(.data$influence_syn_norm)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(influence_norm_log = .data$influence_norm_log-const,
                  influence_log = .data$influence_log-const,
                  influence_syn_norm_log = .data$influence_syn_norm_log-const) %>%
    dplyr::group_by(.data$seed) %>%
    dplyr::mutate(influence_log = ifelse(is.na(.data$influence),0,.data$influence_log)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$target, .data$seed, .keep_all = TRUE) %>%
    dplyr::mutate(influence = signif(.data$influence,4),
                  influence_log = signif(.data$influence_log,4),
                  influence_norm = signif(.data$influence_norm,4),
                  influence_norm_log = signif(.data$influence_norm_log,4)
    ) %>%
    dplyr::distinct(.data$target,
                    .data$seed, 
                    .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(influence = signif(.data$influence,4),
                  influence_log = signif(.data$influence_log,4),
                  influence_norm = signif(.data$influence_norm,4),
                  influence_syn_norm = signif(.data$influence_syn_norm,4),
                  influence_norm_log = signif(.data$influence_norm_log,4),
                  influence_syn_norm_log = signif(.data$influence_syn_norm_log,4)
    ) 
  if(!orig.target){
    influence.df$target <- NULL
  }
  influence.df
}