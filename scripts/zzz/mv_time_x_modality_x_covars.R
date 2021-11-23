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
                  spacing_total, terminators_total), as.integer)) 

modality %>% filter_all(any_vars(is.na(.))) %>%
  glimpse()

modality <- modality %>% drop_na()

# Formulae
f <- bf(advanced_structures ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) +
     bf(events ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) +
     bf(syntax ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) +
     bf(number_words ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) + 
     bf(story_grammar ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = sratio()) +
     bf(vocab_mean_age ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = gaussian()) +
     bf(spelling_correct | trials(number_words) ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = binomial(link = "logit")) + 
     bf(correct_spaces | trials(spacing_total) ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = binomial(link = "logit")) + 
     bf(correct_terminators | trials(terminators_total) ~ poly(time,3) * modality * (discuss_text + discuss_structure + write_story) + (time|s|child:school) + (time|p|school), 
        family = binomial(link = "logit")) 


# Get priors
#get_prior(f + set_rescor(FALSE), data = modality)

# Priors
prior <- set_prior("normal(0, 2)", class = "Intercept", 
                   resp = c("advancedstructures", "events", "syntax")) +
         set_prior("normal(0, 1)", class = "b", 
                   resp = c("advancedstructures", "events", "syntax")) +
         set_prior("normal(2, 2)", class = "Intercept", resp = "numberwords") +
         set_prior("normal(0, 1)", class = "b", resp = "numberwords") +
         set_prior("normal(1, 4)", class = "Intercept", resp = "storygrammar") +
         set_prior("normal(0, 1)", class = "b", resp = "storygrammar") +
         set_prior("normal(7, 1)", class = "Intercept", resp = "vocabmeanage") +
         set_prior("normal(0, 1)", class = "b", resp = "vocabmeanage") +
         set_prior("normal(0, 3)", class = "Intercept", 
                   resp = c("spellingcorrect", "correctspaces", "correctterminators")) +
         set_prior("normal(0, 1)", class = "b", 
            resp = c("spellingcorrect", "correctspaces", "correctterminators"))

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
        file = "stanout/mv_time_x_modality_x_covars.rda", 
        compress = "xz")

