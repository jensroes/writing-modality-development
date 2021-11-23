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

modality %>% 
  filter_all(any_vars(is.na(.))) %>%
  glimpse()

modality <- modality %>% drop_na()

# Formulae
f <- bf(advanced_structures ~ time + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) +
     bf(events ~ time + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) +
     bf(syntax ~ time + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) +
     bf(number_words ~ time + (time|s|child:school) + (time|p|school), 
        family = negbinomial()) + 
     bf(story_grammar ~ time + (time|s|child:school) + (time|p|school), 
        family = sratio()) +
     bf(vocab_mean_age ~ time + (time|s|child:school) + (time|p|school), 
        family = gaussian()) +
     bf(spelling_correct | trials(number_words) ~ time + (time|s|child:school) + (time|p|school), 
        family = binomial(link = "logit")) + 
     bf(correct_spaces | trials(spacing_total) ~ time + (time|s|child:school) + (time|p|school), 
        family = binomial(link = "logit")) + 
     bf(correct_terminators | trials(terminators_total) ~ time + (time|s|child:school) + (time|p|school), 
        family = binomial(link = "logit")) 


# Get priors
get_prior(f + set_rescor(FALSE), data = modality)

# Priors
prior <- set_prior("normal(0, 2)", class = "Intercept", 
                   resp = c( "advancedstructures", "events", "syntax")) +
         set_prior("normal(0, 1)", class = "b", 
                   resp = c( "advancedstructures", "events", "syntax")) +
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
        file = "stanout/mv_time.rda", 
        compress = "xz")

m %>% fixef() %>% as.data.frame() %>% rownames_to_column() %>%
  mutate(keep = grepl(pattern = "time", x =  rowname)) %>%
  rename(DV = rowname) %>% filter(keep) %>% select(-keep) -> model_summary

# Test hypotheses
# null-hypothesis density ratio:
# effect of 0 is x times less likely after seeing the data:

h <- map(model_summary$DV, ~hypothesis(m, paste0(.x, " = 0"))[["hypothesis"]])
model_summary <- map(1:9, ~h[[.x]]) %>%
  bind_rows() %>% as_tibble() %>%
  rename(DV = Hypothesis) %>%
  mutate(DV = gsub(pattern = ") = 0", "", DV),
         DV = gsub(pattern = "\\(", "", DV),
         H0 = Evid.Ratio,
         H1  = 1/H0) %>%
  select(DV, H0, H1) %>% left_join(model_summary, by = "DV") %>%
  separate(DV, into = c("DV", "remove"), sep = "_" ) %>% 
  select(DV, Estimate, Q2.5, Q97.5, H0, H1); model_summary

# Save model results summary
write_csv(model_summary, "stanout/mv_time.csv")
