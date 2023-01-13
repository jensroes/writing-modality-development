# Load packages
library(brms)
library(tidyverse)
library(patchwork) # combining plots
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right"))

# Load posterior
m <- readRDS("stanout/mv_time_x_modality_poly2.rda")

data <- m$data %>%
  mutate(across(c(advanced_structures, events:terminators_total), as.numeric),
         time = time + 1,
         across(correct_spaces, ~./ spacing_total),
         across(correct_terminators, ~./ terminators_total),
         across(spelling_correct, ~./number_words)) %>%
  pivot_longer(c(advanced_structures, events:correct_spaces, correct_terminators)) %>%
  select(time, modality, dv = name, value) %>%
  mutate(dv = str_remove_all(dv, "_")) 

# Extract conditional effects for plotting
ces <- conditional_effects(m, re_formula = NA) 

# Need to extract values for ordinal data differently
#ces2 <- conditional_effects(m, resp = "storygrammar", categorical = T) 
#names(ces2)[grepl(x = names(ces2), pattern = "time:modality")]

# select relevant variables
idx <- names(ces)[grepl(x = names(ces), pattern = "time:modality")]

ps <- map(idx, ~ces[.x] %>%
            bind_cols() %>%
            as_tibble() %>%
            mutate(dv = .x)) %>%
  bind_rows() %>%
  select(dv, time, modality, 
         est = estimate__, 
         lo = lower__, 
         up = upper__,
         spacing_total, number_words, terminators_total) %>%
  separate(dv, into = c("dv", "remove"), sep = "\\.") %>%
  select(-remove) %>%
  mutate(time = time + 1)

write_csv(ps, "stanout/posterior_for_plotting.csv")


data_means <- data %>% group_by(dv, time, modality) %>%
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()),
            lo = mean - 1.96 * se,
            up = mean + 1.96 * se)

ps %>% 
  mutate(across(c(est, lo, up), ~ifelse(dv == "spellingcorrect", . / number_words, 
                                               ifelse(dv == "correctspaces", . / spacing_total, 
                                                      ifelse(dv == "correctterminators", . / terminators_total, .))))) %>% 
  ggplot(aes(x = time, y = est, colour = modality, fill = modality)) +
  geom_ribbon(aes(ymin = lo, ymax = up), colour = NA, alpha =  .1) +
  geom_line(size = 1) +
  geom_point(data = data_means, aes(y = mean), size = 2, position = position_dodge(.5)) +
  geom_errorbar(data = data_means, aes(y = mean, ymin = lo, ymax = up), 
               width = .1, position = position_dodge(.5)) +
  geom_line(data = data_means, aes(y = mean), size = .5, alpha = .5,
            linetype = "dashed", position = position_dodge(.5)) +
  facet_wrap(~dv, scales = "free") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(colour = "Modality", fill = "Modality", x = "Time points", 
       y = "Estimate / sample mean with 95% P/CIs")

