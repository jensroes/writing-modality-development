library(tidyverse)
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right"))

modality <- readRDS(file = "data/modality_development.Rdata")

modality <- modality %>% drop_na()

modality %>%
  mutate(terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
  select(modality, time, contains("terminator")) %>%
  pivot_longer(additional_terminators:terminator_accuracy) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")



modality %>%
  mutate(terminator_errors = (additional_terminators + missing_terminators) ) %>%
#  mutate(terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
  select(modality, time, contains("terminator")) %>%
  pivot_longer(additional_terminators:terminator_errors) %>%
#  filter(name != "terminator_accuracy") %>%
  group_by(modality, time, name) %>%
  summarise(total = sum(value),
            n = n(),
            prop = total / n) %>%
  ggplot(aes(y = total, x = time, colour = modality)) +
  geom_point() +
#  geom_line() +
  geom_smooth(aes(x = as.numeric(time)), method = "lm", se = F, formula = y ~ poly(x,2)) +
  facet_wrap(~name, scales = "free")


modality %>%
  mutate(terminator_errors = (additional_terminators + missing_terminators) ) %>%
  #  mutate(terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
  select(modality, time, contains("terminator")) %>%
  pivot_longer(additional_terminators:terminator_errors) %>%
  #  filter(name != "terminator_accuracy") %>%
  ggplot(aes(y = value, x = time, colour = modality)) +
  geom_jitter() +
  #  geom_line() +
  geom_smooth(aes(x = as.numeric(time)), method = "lm", se = F, formula = y ~ poly(x,2)) +
  facet_wrap(~name, scales = "free")



modality %>%
  mutate(space_errors = (additional_spaces + missing_spaces) ) %>%
  #  mutate(terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
  select(modality, time, contains("space")) %>%
  pivot_longer(correct_spaces:space_errors) %>%
  group_by(modality, time, name) %>%
  summarise(total = sum(value),
            n = n(),
            prop = total / n) %>%
  ggplot(aes(y = total, x = time, colour = modality)) +
  geom_point() +
  #  geom_line() +
  geom_smooth(aes(x = as.numeric(time)), method = "lm", se = F, formula = y ~ poly(x,2)) +
  facet_wrap(~name, scales = "free")
