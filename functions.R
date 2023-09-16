library(ggplot2)
library(truncnorm)
library(brms)

# synthetic data function
syntdat <- function(N_groups   = 3,
                    N_samples  = c(6,6,10),
                    effects    = c(.3,-.2), 
                    sd_samples = .1,
                    sd_dups    = .05,
                    seed       = 12432) {
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

# get and plot data
ds <- syntdat()
ggplot(ds, 
       aes(x = Group,
           y = Value,
           col = Dupl)) +
  geom_point(position = position_dodge(width = 0.5)) +
  ylim(0,NA) +
  theme_bw()

# model data
# f <- brmsformula(Value ~ Group + (1|Sample))
# get_prior(f, data = ds)
# fit <- brm(f, data = ds, 
#            prior = c(prior(normal(0,1), class = Intercept),
#                      prior(normal(1,1), class = b)),
#            chains = 4,
#            cores = 4,
#            iter = 5000)
# prior_summary(fit)
fit <- update(fit, 
              newdata = ds, 
              chains = 4,
              cores = 4,
              iter = 5000)
fit
