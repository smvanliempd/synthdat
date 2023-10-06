library(ggplot2)
library(brms)
### analysis ###

# make data
source("functions.R")
ds <- syntdat(n_Run = 3,
              n_Treatment = 3,
              n_Sample = 6,
              n_Repl = 2,
              b_Treatment = c(1.0, 0.7, 1.2),
              sigma_plate = 0.1,
              sigma_repl = 0.03,
              p_sigma_sample = 25,
              seed = 10 )

# plot data
plot_dat(ds$data)

# check some stuff
mean(ds$sigma_sample)
sd(ds$z_Sample)
sd(ds$z_Plate)

# random-effects model
f01 <- brmsformula(Value ~ 0 + Treatment + (1|Plate) + (1|Sample) )
(ps01 <- get_prior(f01, data = ds$data))
ps01$prior[2:4]    <- "normal(1,0.25)"
ps01$prior[c(7,9)] <- "normal(0,0.25)"
make_stancode(f01,data = ds$data,prior = ps01)
fit01 <- brm(f01, data = ds$data, prior = ps01, chains = 0, save_pars = save_pars(all = TRUE) ) #compile only
fit01.1 <- update(fit01, newdata = ds$data, chains = 4, cores = 4, iter = 8000,control = list(adapt_delta = 0.99, max_treedepth = 15))
fit01
fit01.1

# random-effects model
f02 <- brmsformula(Value ~ 0 + Treatment + (1|Plate) + (1|gr(Sample, by = Plate)))
(ps02 <- get_prior(f02, data = ds$data))
ps02$prior[2:4]    <- "normal(1,0.25)"
ps02$prior[c(7,9)] <- "normal(0,0.25)"
make_stancode(f02,data = ds$data,prior = ps02)
fit02 <- brm(f02, data = ds$data, prior = ps02, chains = 0, save_pars = save_pars(all = TRUE) ) #compile only
fit02 <- update(fit02, newdata = ds$data, chains = 4, cores = 4, iter = 8000,control = list(adapt_delta = 0.99, max_treedepth = 15))
fit02

