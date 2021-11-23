rm(list=ls())
library(tidyverse)
library(brms)

# Model fit params
nchains <- ncores <- 3
niter <- 10000
warmup <- niter/2

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  drop_na() %>%
  select(child, school, modality, time, number_words, correct_terminators)

# Formulae
f <- bf(correct_terminators ~ poly(time, 2) * modality * number_words + 
          (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
        family = negbinomial())

# Get priors
get_prior(f, data = modality)

# Priors
prior <- set_prior("student_t(2, 0, 2)", class = "b", coef = "modalitytyping") +
         set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime21") +
         set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime22") +
         set_prior("student_t(5, 0, 2)", class = "b", coef = "number_words") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime21:modalitytyping") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime22:modalitytyping") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime21:number_words") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime22:number_words") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "modalitytyping:number_words") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime21:modalitytyping:number_words") +
         set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime22:modalitytyping:number_words") 
  

# Sampling
m <- brm(f, 
         data = modality, 
         prior = prior,
         inits = 0,
         warmup = warmup,
         iter = niter, 
         save_pars = save_pars(all = TRUE),
         sample_prior = TRUE,
         refresh = niter/4,
         chains = nchains,
         cores = ncores,
         control = list(adapt_delta = 0.99, max_treedepth = 16))

# Save model
saveRDS(m, 
        file = "stanout/terminators_correct_words_time_x_modality_poly2.rda", 
        compress = "xz")

conditional_effects(m)

