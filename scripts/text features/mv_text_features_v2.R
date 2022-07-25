rm(list=ls())
library(tidyverse)
library(brms)

# Model fit params
nchains <- ncores <- 3
niter <- 10000
warmup <- niter/2

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  mutate(condition = paste0(modality, "_time ", time + 1 ),
         number_spaces = correct_spaces + additional_spaces,
         number_terminators = correct_terminators + additional_terminators,
        #story_grammar = ordered(story_grammar),
         across(c(number_words, number_spaces, number_terminators), as.integer)) %>% 
  select(child, school, condition, time, starts_with("number_")) %>%
  drop_na() 

glimpse(modality)

pivot_longer(modality, starts_with("number_")) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")

# Formulae
f <- bf(number_words ~ 0 + condition + 
          (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
        family = negbinomial()) + 
     bf(number_spaces ~ 0 + condition + 
       (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
       family = negbinomial()) + 
     bf(number_terminators ~ 0 + condition + 
       (poly(time, 1)|s|child:school) + (poly(time, 1)|p|school), 
       family = negbinomial())  

# Get priors
#get_prior(f + set_rescor(FALSE), data = modality)
dvs <- c("numberwords", "numberspaces", "numberterminators")
prior <- set_prior("student_t(5, 0, 10)", class = "b", resp = dvs) 

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
         control = list(adapt_delta = 0.99, 
                        max_treedepth = 16))

# Save model
saveRDS(m, 
        file = "stanout/mv_text_features_v2.rda", 
        compress = "xz")

conditional_effects(m)
