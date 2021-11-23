rm(list=ls())
library(tidyverse)
library(brms)

# Model fit params
nchains <- ncores <- 3
niter <- 10000
warmup <- niter/2

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>% drop_na()

# Formulae
f <- bf(additional_terminators ~ poly(time, 2) * modality + 
          (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
        family = negbinomial()) +
     bf(correct_terminators ~ poly(time, 2) * modality + 
          (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
        family = negbinomial()) +
     bf(missing_terminators ~ poly(time, 2) * modality + 
          (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
        family = negbinomial()) 


# Get priors
#get_prior(f + set_rescor(FALSE), data = modality)

# Priors
dvs <- c("additionalterminators", "correctterminators", "missingterminators")
prior <- set_prior("student_t(2, 0, 2)", class = "b", coef = "modalitytyping", resp = dvs) +
         set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime21", resp = dvs) +
         set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime22", resp = dvs) +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime21:modalitytyping", resp = dvs) +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime22:modalitytyping", resp = dvs) 

# Sampling
m <- brm(f + set_rescor(FALSE), 
         data = modality, 
         prior = prior,
         inits = 0,
         warmup = warmup,
         iter = niter, 
         sample_prior = TRUE,
         refresh = niter/4,
         chains = nchains,
         cores = ncores,
         control = list(adapt_delta = 0.99, max_treedepth = 16))

# Save model
saveRDS(m, 
        file = "stanout/mv_terminators_time_x_modality_poly2.rda", 
        compress = "xz")

conditional_effects(m)
