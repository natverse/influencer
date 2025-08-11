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
    dplyr::mutate(no_seeds = influence_original/influence_norm_original,
                  no_seeds = ifelse(is.na(no_seeds),1,no_seeds),
                  no_synapses = influence_original/influence_syn_norm) %>%
    dplyr::group_by(target) %>%
    dplyr::mutate(no_targets = length(unique(id))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      no_seeds = as.numeric(no_seeds),
      no_targets = as.numeric(no_targets)
    ) %>%
    dplyr::group_by(seed) %>%
    dplyr::mutate(influence_per_seed = influence_original*no_seeds,
                  influence_per_synapse = influence_original*no_synapses) %>%
    dplyr::group_by(target, seed) %>%
    dplyr::mutate(influence = sum(influence_original,na.rm = TRUE),
                  total_seeds = sum(no_seeds,na.rm=TRUE),
                  total_synapses = sum(no_synapses,na.rm=TRUE),
                  influence_norm = sum(influence_per_seed,na.rm = TRUE)/(total_seeds*no_targets),
                  influence = ifelse(influence<inf.threshold,inf.threshold,influence),
                  influence_norm = ifelse(influence_norm<inf.threshold,inf.threshold,influence_norm),
                  influence_norm = sum(influence_norm,na.rm = TRUE),
                  influence_syn_norm =  sum(influence_per_synapse,na.rm = TRUE)/(no_targets*total_synapses),
                  influence_syn_norm = ifelse(influence_syn_norm<inf.threshold,inf.threshold,influence_syn_norm),
                  influence_syn_norm = sum(influence_syn_norm,na.rm = TRUE),
                  influence_norm_log = log(influence_norm),
                  influence_log = log((influence/no_targets)),
                  influence_syn_norm_log = log(influence_syn_norm)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(influence_norm_log = influence_norm_log-const,
                  influence_log = influence_log-const,
                  influence_syn_norm_log = influence_syn_norm_log-const) %>%
    dplyr::group_by(seed) %>%
    dplyr::mutate(influence_log = ifelse(is.na(influence),0,influence_log)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(target, seed, .keep_all = TRUE) %>%
    dplyr::mutate(influence = signif(influence,4),
                  influence_log = signif(influence_log,4),
                  influence_norm = signif(influence_norm,4),
                  influence_norm_log = signif(influence_norm_log,4)
    ) %>%
    dplyr::distinct(target,
                    seed, 
                    .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(influence = signif(influence,4),
                  influence_log = signif(influence_log,4),
                  influence_norm = signif(influence_norm,4),
                  influence_syn_norm = signif(influence_syn_norm,4),
                  influence_norm_log = signif(influence_norm_log,4),
                  influence_syn_norm_log = signif(influence_syn_norm_log,4)
    ) 
  if(!orig.target){
    influence.df$target <- NULL
  }
  influence.df
}