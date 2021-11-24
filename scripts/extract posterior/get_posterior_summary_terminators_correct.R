# Load packages
library(brms)
library(tidyverse)

# Load posterior
m <- readRDS("stanout/terminators_correct_words_time_x_modality_poly2.rda")

# Extract summary
m %>% fixef() %>% as.data.frame() %>% rownames_to_column() %>%
  mutate(keep = !grepl(pattern = "Intercept", x =  rowname)) %>%
  rename(DV = rowname) %>% filter(keep) %>% select(-keep) -> model_summary

model_summary %>%
  mutate(across(where(is.numeric), round, 2))

# Test hypotheses
# null-hypothesis density ratio:
# effect of 0 is x times less likely after seeing the data:

h <- map(model_summary$DV, ~hypothesis(m, paste0(.x, " = 0"))[["hypothesis"]])
model_summary_bfs <- map(seq(length(h)), ~h[[.x]]) %>%
  bind_rows() %>% as_tibble() %>%
  rename(DV = Hypothesis) %>%
  mutate(DV = gsub(pattern = ") = 0", "", DV),
         DV = gsub(pattern = "\\(", "", DV),
         H0 = Evid.Ratio,
         H1  = 1/H0) %>%
  select(DV, H0, H1) %>% left_join(model_summary, by = "DV") %>%
  select(Pred = DV, Estimate, Q2.5, Q97.5, H0, H1)

model_summary_bfs %>% mutate(across(where(is.numeric), round, 2))

# Save model results summary
write_csv(model_summary_bfs, "stanout/posterior_summary_terminators_correct.csv")

