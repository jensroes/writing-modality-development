# Load packages
library(brms)
library(tidyverse)

# Load posterior
m <- readRDS("stanout/mv_terminators_time_x_modality_poly2.rda")

# Extract conditional effects for plotting
ces <- conditional_effects(m, re_formula = NA) 

# select relevant variables
idx <- names(ces)[grepl(x = names(ces), pattern = "time:modality")]

ps <- map(idx, ~ces[.x] %>%
            bind_cols() %>%
            as_tibble() %>%
            mutate(dv = .x)) %>%
  bind_rows() %>%
  select(dv, time, modality, est = estimate__, lo = lower__, up = upper__) %>%
  separate(dv, into = c("dv", "remove"), sep = "\\.") %>%
  select(-remove) %>%
  mutate(time = time + 1)

write_csv(ps, "stanout/posterior_for_plotting_terminators.csv")
