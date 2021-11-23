rm(list=ls())
library(tidyverse)
library(brms)

# Model fit params
nchains <- ncores <- 3
niter <- 10000
warmup <- niter/2

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  mutate(story_grammar = ordered(story_grammar),
         spacing_total = correct_spaces + additional_spaces + missing_spaces,
         terminators_total = correct_terminators + additional_terminators + missing_terminators,
         across(c(advanced_structures, events, syntax, number_words, 
                  spelling_correct, correct_spaces, 
                  spacing_total, terminators_total), as.integer)) %>% 
  drop_na()

# Formulae
f <- bf(spelling_correct | trials(number_words) ~ poly(time,2) + modality + (poly(time,1)|s|child:school) + (poly(time,1)|p|school), 
        family = binomial(link = "logit"))

# Priors
prior <- set_prior("student_t(2, 0, 2)", class = "b", coef = "modalitytyping") +
        set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime21") + 
        set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime22") 

# Get priors
#get_prior(f + set_rescor(FALSE), data = modality)

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
        file = "stanout/spelling_time_modality_poly2.rda", 
        compress = "xz")

mcmc_plot(m, type = "trace")
conditional_effects(m)
