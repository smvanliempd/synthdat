library(ggplot2)
library(truncnorm)
library(brms)

### analysis ###

# make data
source("functions.R")
ds <- syntdat(N_groups   = 3,
              N_samples  = c(6,6,6),
              effects    = c(.3,-.2),
              sd_samples = .1,
              sd_dups    = c(.05, .01, 0.09))

# plot data
plot_dat(ds)

# model data
f00 <- brmsformula(Value ~ Treatment + (1|Sample))
(pri00 <- get_prior(f00, data = ds))
pri00$prior[1:3] <- "normal(0,1)"
pri00$prior[4] <- "normal(1,1)"
pri00$prior[5:8] <- "exponential(1)"
fit00 <- brm(f00, data = ds,
             prior = pri00,
             chains = 4,
             cores = 4,
             iter = 5000)
fit00
prior_summary(fit00)
posterior_summary(fit00)

# fit00.1 <- update(fit00,
#               newdata = ds,
#               chains = 4,
#               cores = 4,
#               iter = 5000)
# fit00.1

f01 <- brmsformula( Value ~ Treatment + (1|gr(Sample, by = Treatment)))
(pri01 <- get_prior(f01, data = ds))
pri01$prior[1:3] <- "normal(0,1)"
pri01$prior[4] <- "normal(1,1)"
pri01$prior[5:8] <- "exponential(1)"
fit01 <- brm(f01, data = ds,
             prior = pri01,
             chains = 4,
             cores = 4,
             iter = 5000)
fit01
prior_summary(fit01)
posterior_summary(fit01)
