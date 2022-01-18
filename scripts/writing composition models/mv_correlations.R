rm(list=ls())
library(tidyverse)
library(brms)

# Model fit params
nchains <- ncores <- 3
niter <- 20000
warmup <- niter/2

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  mutate(spacing_total = correct_spaces + additional_spaces + missing_spaces,
         terminators_total = correct_terminators + additional_terminators + missing_terminators,
         spelling_accuracy = spelling_correct / number_words,
         spacing_accuracy = correct_spaces / spacing_total,
         terminator_accuracy = correct_terminators / terminators_total,
         across(ends_with("_accuracy"), ~replace( ., . == 0, 0.01)),
         across(ends_with("_accuracy"), ~replace( ., . == 1, 0.99)),
         across(ends_with("_accuracy"), logit_scaled)) %>% drop_na() 

glimpse(modality)

# Formulae
f <- bf(mvbind(advanced_structures, events, 
               syntax, number_words, story_grammar,
               vocab_mean_age, spelling_accuracy, 
               spacing_accuracy, terminator_accuracy) ~ 1 + 
          (poly(time, 1)|s|child:school) + 
          (poly(time, 1)|p|school), 
        family = gaussian()) 

# Get priors
#get_prior(f + set_rescor(TRUE), data = modality) %>%
#  as_tibble() %>%
#  select(prior, class, resp, coef, group) %>%
#  arrange(class, resp) %>%
#  filter(str_detect(resp, "accuracy")) %>%
#  separate(prior,into = c("df", "mean", "scale"), sep = ", ") %>%
#  drop_na() %>%
#  transmute(mean = parse_number(mean),
#            param = paste(class, resp, sep = "_"),
#            init = paste0(param, " = ", mean)) %>%
#  pull(init) %>% paste(collapse = ", ")

#get_prior(f + set_rescor(TRUE), data = modality) %>%
#  as_tibble() %>%
#  mutate(prior = ifelse(class %in% c("rescor", "cor"), "lkj(3)", prior),
#         prior = ifelse(class == "sd" & prior == "", "student_t(7, 0, 3)", prior)) %>%
#  select(prior, class, resp, coef, group, resp) %>%
#  filter(prior != "") %>%
#  arrange(class, resp) %>% 
#  transmute(prior = paste0('set_prior("', prior, '", class="', class, '", resp = "', resp, '", coef = "', coef, '", group = "', group,'") +')) %>%
#  pull(prior) %>% cat()

# Priors
prior <- set_prior("lkj(3)", class="cor", resp = "", coef = "") + 
         set_prior("lkj(3)", class="cor", resp = "", coef = "", group = "child:school") + 
         set_prior("lkj(3)", class="cor", resp = "", coef = "", group = "school") + 
  set_prior("student_t(3, 3, 2)", class="Intercept", resp = "advancedstructures", coef = "") +
  set_prior("student_t(3, 3, 2)", class="Intercept", resp = "events", coef = "") + 
  set_prior("student_t(3, 24, 8.3)", class="Intercept", resp = "numberwords", coef = "") + 
  set_prior("student_t(3, 2.8, .5)", class="Intercept", resp = "spacingaccuracy", coef = "") +
  set_prior("student_t(3, 0.8, .5)", class="Intercept", resp = "spellingaccuracy", coef = "") + 
  set_prior("student_t(3, 1, 1.5)", class="Intercept", resp = "storygrammar", coef = "") + 
  set_prior("student_t(3, 4.5, 1.7)", class="Intercept", resp = "syntax", coef = "") + 
  set_prior("student_t(3, -2, 1)", class="Intercept", resp = "terminatoraccuracy", coef = "") + 
  set_prior("student_t(3, 6.9, 1.5)", class="Intercept", resp = "vocabmeanage", coef = "") + 
  set_prior("lkj(3)", class="rescor", resp = "", coef = "")  

# Sampling
m <- brm(f + set_rescor(TRUE), 
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
        file = "stanout/mv_correlations.rda", 
        compress = "xz")

m %>% fixef() %>% round(2)
