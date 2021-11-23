# Load packages
library(brms)
library(tidyverse)

# Load posterior
m <- readRDS("stanout/terminators_correct_words_time_x_modality_poly2.rda")

# Extract conditional effects for plotting
ces <- conditional_effects(m, re_formula = NA) 

# select relevant variables
idx <- names(ces)[grepl(x = names(ces), pattern = "time:modality", fixed = T)]
ps <- ces[idx] %>%
  bind_cols() %>%
  as_tibble() %>%
  select(time, modality, est = estimate__, lo = lower__, up = upper__) %>%
  mutate(time = time + 1)

write_csv(ps, "stanout/posterior_for_plotting_terminators_correct.csv")
