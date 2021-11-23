library(tidyverse)
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right"))

modality <- readRDS(file = "data/modality_development.Rdata")

modality <- modality %>% drop_na()


modality %>%
  mutate(spacing_accuracy = correct_spaces / (correct_spaces + additional_spaces + missing_spaces),
         terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators),
         spelling_correct = spelling_correct/number_words) %>%
  select(-ends_with("spaces"),-ends_with("terminators")) %>%
  pivot_longer(number_words:terminator_accuracy) %>%
  filter(name != "age") %>%
#  filter(str_detect(name, "accuracy")) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")

modality %>%
  mutate(spelling_correct = spelling_correct/number_words,
    spacing_accuracy = correct_spaces / (correct_spaces + additional_spaces + missing_spaces),
    terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
#  select(correct_terminators, additional_terminators, missing_terminators) %>% as.data.frame()
  #glimpse()
  select(-ends_with("spaces"),-ends_with("terminators"), -starts_with("discuss_"), -teacher_id, -write_story) %>%
  pivot_longer(number_words:terminator_accuracy) %>%
  group_by(modality, time, name) %>%
  summarise(mean = mean(value, na.rm = T),
            se = sd(value, na.rm = T)/sqrt(n())) %>%
  filter(name != "age") %>%
  ggplot(aes(y = mean, 
             x = factor(time),
             ymin = mean - 1.96*se,
             ymax = mean + 1.96*se,
             group = interaction(modality ))) +
  geom_line(alpha = .5) +
  geom_point(aes(colour = modality)) +
  geom_errorbar(width = .1, alpha = .5) +
  facet_wrap(~name, scales = "free") +
  labs(y = "mean score with 1.96 * SEs", x = "time")


summary <- modality %>%
  mutate(spacing_accuracy = correct_spaces / (correct_spaces + additional_spaces + missing_spaces),
         terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
  select(-ends_with("spaces"),-ends_with("terminators")) %>%
  pivot_longer(number_words:terminator_accuracy) %>%
  filter(name != "age") %>%
  group_by(modality, time, name) %>%
  summarise(mean = mean(value, na.rm = T),
            se = sd(value, na.rm = T)/sqrt(n())) 


modality %>%
  mutate(spacing_accuracy = correct_spaces / (correct_spaces + additional_spaces + missing_spaces),
         terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators),
         spelling_correct = spelling_correct/number_words) %>%
  select(-ends_with("spaces"),-ends_with("terminators")) %>%
  pivot_longer(number_words:terminator_accuracy) %>%
  filter(name != "age") %>%
  group_by(modality, time, name) %>%
  ggplot(aes(y = value, 
             x = factor(time),
             group = interaction(modality ))) +
  geom_point(aes(colour = modality),
             size = .25, alpha = .25,
             position = position_jitterdodge(jitter.width = .1, 
                                             jitter.height = 0,
                                             dodge.width = .5)) +
  geom_point(data = summary, inherit.aes = F, 
             aes(x = factor(time), y = mean, colour = modality), 
             shape = 21, size = 3,
             position = position_dodge(.5)) +
  geom_errorbar(data = summary, inherit.aes = F,
              aes(x = factor(time), 
                  ymin = mean - 1.96*se, 
                  ymax = mean + 1.96*se,
                  colour = modality), 
    width = .1, alpha = .5, position = position_dodge(.5)) +
  facet_wrap(~name, scales = "free") +
  labs(y = "mean score with 1.96 * SEs", x = "time") +
  scale_colour_brewer(palette = "Dark2")


modality %>%
  mutate(terminator_accuracy = correct_terminators / (correct_terminators + additional_terminators + missing_terminators)) %>%
  select(text_id, contains("terminator"), time, modality, number_words) %>%
  filter(time == 1) %>% select(-time) %>%
#  unique() %>%
  filter(terminator_accuracy > .5) %>%
  mutate(terminator_accuracy = round(terminator_accuracy,2)) %>%
  as.data.frame()

