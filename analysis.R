library(ggplot2)
library(truncnorm)
library(brms)

### analyis ###

# make data
source("functions.R")
ds <- syntdat(N_groups   = 3,
              N_samples  = c(6,6,6),
              effects    = c(.3,-.2),
              sd_samples = .1,
              sd_dups    = .05)

# plot data
plot_dat(ds)

# model data
f <- brmsformula(Value ~ Group + (1|Sample))
# get_prior(f, data = ds)
fit00 <- brm(f, data = ds,
           prior = c(prior(normal(0,1), class = Intercept),
                     prior(normal(1,1), class = b)),
           chains = 4,
           cores = 4,
           iter = 5000)
# prior_summary(fit)
fit00

fit01 <- update(fit00,
              newdata = ds,
              chains = 4,
              cores = 4,
              iter = 5000)
fit01
