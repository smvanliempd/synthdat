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
# random-effects model
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

# random-effects model, allowing for varying sigmas for repeated measures in treatment groups.
# in order to get sigmas for replicates in groups: exp(sigma_Intercept) * exp(sigma_TreatmentX)
f02 <- brmsformula( Value ~ Treatment + (1|Sample),
                    sigma ~ Treatment)
(pri02 <- get_prior(f02, data = ds))
pri02$prior[2:3] <- "normal(0,0.5)"
pri02$prior[4]   <- "normal(1,0.5)"
pri02$prior[5]   <- "exponential(1)"
pri02$prior[9:11] <- "student_t(3, 0, 2.5)"
(pri02 <- pri02[ pri02$prior != "", ])
fit02 <- brm(f02, data = ds,
             prior = pri02,
             chains = 4,
             cores = 4,
             iter = 5000,
             control = list(adapt_delta = 0.9))
fit02
fit02.1 <- update(fit02,
                  chains = 4,
                  cores = 4,
                  iter = 5000,
                  control = list(adapt_delta = 0.99))
fit02.1





