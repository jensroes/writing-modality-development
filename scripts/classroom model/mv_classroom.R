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

glimpse(modality)

count(modality, modality, discuss_text)
count(modality, modality, discuss_structure)
count(modality, modality, write_story)

# Formulae
f <- bf(discuss_text ~ 1, family = cumulative()) +
     bf(discuss_structure ~ 1, family = cumulative()) +
     bf(write_story ~ 1, family = cumulative())

# Sampling
m <- brm(f + set_rescor(FALSE), 
         data = modality,
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
        file = "stanout/mv_classroom.rda", 
        compress = "xz")


mcmc_plot(m, type = "trace")