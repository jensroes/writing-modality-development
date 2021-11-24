# Load packages
library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork) # combining plots
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right"))

# Load posterior
m <- readRDS("stanout/mv_time_x_modality_poly2.rda")
#m <- readRDS("stanout/mv_time_x_modality_x_covars_poly2.rda")

data <- m$data %>%
  mutate(across(c(advanced_structures, events:terminators_total), as.numeric),
         time = time + 1) %>%
  pivot_longer(c(advanced_structures, events:terminators_total)) %>%
  select(time, modality, dv = name, value) %>%
  mutate(dv = str_remove_all(dv, "_")) %>%
  filter(!grepl(x = dv, pattern = "total"))


# Posterior predictions across child
newdata <- expand_grid(modality = unique(data$modality),
                       time = unique(data$time))

storygrammar <- epred_draws(m, newdata = newdata, re_formula = NA, resp = "storygrammar") %>%
  select(modality, time, .epred, .category) %>%
  group_by(modality, time, .category) %>%
  summarise(mean = mean(.epred),
            lo = quantile(.epred, .025),
            up = quantile(.epred, .975)) 

storygrammar %>%
  ggplot(aes(y = mean, ymin = lo, ymax = up, 
             fill = modality, 
             colour = modality, x = time)) +
  geom_ribbon(alpha = .25, colour = NA) +
  geom_line() +
  facet_wrap(~.category)


newdata %>%
  add_epred_draws(m, re_formula = NA, resp = "storygrammar") %>%
  median_qi(.epred)

library(modelr)

m$data %>%
  data_grid(time = seq_range(time, n = 5),
            modality = c("typing", "handwriting")) %>%
  add_epred_draws(m, value = "P(rating | time)", category = "storygrammar", re_formula = NA, resp = "storygrammar") %>%
  ggplot(aes(x = time, y = `P(rating | time)`, color = storygrammar)) +
  stat_lineribbon(aes(fill = storygrammar), alpha = 1/5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~modality)

newdata <- expand_grid(modality = unique(data$modality),
                       time = unique(data$time),
                       terminators_total = unique(m$data$terminators_total))

pred <- predicted_draws(m, newdata = newdata, re_formula = NA, resp = "correctterminators")
pred %>%
  ungroup() %>%
  filter(.prediction > 0 ) %>%
  group_by(modality, time, terminators_total) %>%
  summarise(prop = sum(.prediction))



m %>%
  spread_draws(terminators, regex = T) %>%
  mutate(MSESC = list(seq(-0.77, 1.49, 0.01))) %>% #the observed value range of MSESC
  unnest(MSESC) %>%
  mutate(pred = exp(b_Intercept + b_MSESC*MSESC)/(1+exp(b_Intercept + b_MSESC*MSESC))) %>%
  group_by(MSESC) %>%
  summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = MSESC, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha=0.2) +
  ylab("Predicted Probability of Repeating a Grade") +
  scale_y_continuous(breaks = seq(0, 0.22, 0.01))



pred
select(modality, time, .epred) %>%
  group_by(modality, time) %>%
  summarise(median = mean(.epred),
            lo = quantile(.epred, .025),
            up = quantile(.epred, .975)) %>%
  ggplot(aes(y = median, ymin = lo, ymax = up, fill = modality, x = time)) +
  geom_ribbon(alpha = .25)



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
  select(dv, time, modality, est = estimate__, lo = lower__, up = upper__) %>%
  separate(dv, into = c("dv", "remove"), sep = "\\.") %>%
  select(-remove) %>%
  mutate(time = time + 1)

write_csv(ps, "stanout/posterior_for_plotting.csv")

data_means <- data %>% group_by(dv, time, modality) %>%
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()),
            lo = mean - 1.96 * se,
            up = mean + 1.96 * se)

ggplot(ps, aes(x = time, y = est, colour = modality, fill = modality)) +
  geom_ribbon(aes(ymin = lo, ymax = up), colour = NA, alpha =  .1) +
  geom_line(size = 1) +
#  geom_point(data = data_means, aes(y = mean), size = 2, position = position_dodge(.5)) +
 # geom_errorbar(data = data_means, aes(y = mean, ymin = lo, ymax = up), 
  #             width = .1, position = position_dodge(.5)) +
#  geom_line(data = data_means, aes(y = mean), size = .5, alpha = .5,
#            linetype = "dashed", position = position_dodge(.5)) +
  facet_wrap(~dv, scales = "free") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(colour = "Modality", fill = "Modality", x = "Time points", 
       y = "Estimate / sample mean with 95% P/CIs")

