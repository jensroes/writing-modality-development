rm(list=ls())
library(tidyverse)
library(brms)

# Model fit params
nchains <- ncores <- 3
niter <- 10000
warmup <- niter/2

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  mutate(across(c(discuss_text, discuss_structure, write_story), as.ordered)) %>% 
  drop_na() %>%
  select(school, discuss_text, discuss_structure, write_story, modality) %>%
  unique()

# Formulae
f <- bf(discuss_text ~ modality, family = cumulative()) +
     bf(discuss_structure ~ modality, family = cumulative()) +
     bf(write_story ~ modality, family = cumulative())

# Priors
prior <- set_prior("student_t(3, 0, 2.5)", class = "b", 
                   resp = c("discusstext", "discussstructure", "writestory")) 

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
        file = "stanout/mv_classroom_modality.rda", 
        compress = "xz")


mcmc_plot(m, type = "trace")
