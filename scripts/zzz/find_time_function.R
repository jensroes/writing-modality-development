# Load packages
library(tidyverse)
library(patchwork) # combining plots
theme_set(theme_bw())

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  mutate(story_grammar = ordered(story_grammar),
         spacing = correct_spaces / (correct_spaces + additional_spaces + missing_spaces),
         terminators = correct_terminators / (correct_terminators + additional_terminators + missing_terminators),
         spelling = spelling_correct / number_words) %>%
  select(text_id:number_words, vocab_mean_age:advanced_structures, spacing, terminators, spelling) %>%
  mutate(across(c(number_words:spelling), as.integer)) %>% drop_na()

data <- modality %>%
  pivot_longer(c(number_words:spelling)) %>%
  select(time, modality, dv = name, value) 

data_means <- data %>% group_by(dv, time) %>%
  summarise(mean = mean(value))

p1 <- ggplot(data, aes(x = time, y =  value)) +
  geom_point(data = data_means, aes(y = mean), size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), 
              linetype = "longdash", alpha = .25, size = .5) +
  labs(title = "Poly 2nd order") +
  facet_wrap(~dv, scales = "free") 

p7 <- ggplot(data, aes(x = time, y =  value)) +
  geom_point(data = data_means, aes(y = mean), size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3), 
              linetype = "longdash", alpha = .25, size = .5) +
  labs(title = "Poly 3rd order") +
  facet_wrap(~dv, scales = "free") 

p2 <- ggplot(data, aes(x = time, y =  value)) +
  geom_point(data = data_means, aes(y = mean), size = 2) +
  labs(title = "Exponential function") +
  geom_smooth(method = "lm", formula = y ~ exp(x), 
              linetype = "longdash", alpha = .25, size = .5) +
  facet_wrap(~dv, scales = "free") 


p3 <- ggplot(data, aes(x = time, y =value)) +
  geom_point(data = data_means, aes(y = mean), size = 2) +
  labs(title = "log function") +
  geom_smooth(method = "lm", formula = y ~ log(x + 1), 
              linetype = "longdash", alpha = .25, size = .5) +
  facet_wrap(~dv, scales = "free") 

p6 <- ggplot(data, aes(x = time, y =  value)) +
  geom_point(data = data_means, aes(y = mean), size = 2) +
  labs(title = "Loess model") +
  geom_smooth(alpha = .25) +
  facet_wrap(~dv, scales = "free") 

p4 <- ggplot(data, aes(x = time, y =  value)) +
  geom_point(data = data_means, aes(y = mean), size = 2) +
  labs(title = "Linear model") +
  geom_smooth(method = "lm",  linetype = "longdash", 
              formula = y ~ poly(x,1),
              alpha = .25) +
  facet_wrap(~dv, scales = "free") 


#p5 <- ggplot(data_means, aes(x = time, y =  mean)) +
#  geom_point(size = 2) +
#  geom_path(linetype = "longdash", alpha = .25) +
#  labs(title = "Factor model") +
#  facet_wrap(~dv, scales = "free") 

p4 + p1 + p7 + p2 + p3 + p6


