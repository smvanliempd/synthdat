
# synthetic data function
syntdat <- function(N_groups, N_samples, effects, sd_samples, sd_dups, seed = 12432) {
  set.seed(seed)
  effects <- c(0,effects)
  d <- sapply(1:N_groups, function(i) {
    v <- rtruncnorm(N_samples[i], a = 0, mean = 1 + effects[i], sd = sd_samples)
    vv <- sapply(1:N_samples[i], function(j){ 
      data.frame(Group = LETTERS[i],
                 Sample = paste0(LETTERS[i],j),
                 Dupl = as.character(1:2),
                 Value = rtruncnorm(2, a = 0, mean = v[j], sd = sd_dups))
    }, simplify = FALSE )
    vv <- do.call(rbind,vv)
  },simplify = FALSE)
  d <- do.call(rbind, d)
}

