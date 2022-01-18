library(brms)
library(tidyverse)
ilogit <- function(x) {
  1 / (1 + exp(-x))
}
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

# Load posterior
m <- readRDS("stanout/mv_correlations.rda")

# Get estimates for residual correlations
rescor <- summary(m)$rescor %>%
  as.data.frame() %>%
  rownames_to_column("pred") %>%
  as_tibble() %>%
  select(pred, cor_est = Estimate, cor_lo = `l-95% CI`, cor_up = `u-95% CI`) %>%
  separate(pred, into = c("x", "y"), sep = ",") %>%
  mutate(x = str_remove(x, "rescor\\("),
         y = str_remove(y, "\\)"))

# Get fixed effect estimates for every DV
fixef <- fixef(m) %>%
  as.data.frame() %>%
  rownames_to_column("pred") %>%
  as_tibble() %>%
  select(pred, dv_est = Estimate, dv_lo = `Q2.5`, dv_up = `Q97.5`) %>%
  mutate(pred = str_remove(pred, "_Intercept"))

# Get a plot of correlations
rescor %>% 
  mutate(label = str_remove(round(cor_est, 2), "^0+")) %>% 
  #  filter(dv_x == "transitiondur") %>%
  ggplot(aes(x = x, y = y, fill = cor_est, label = label )) +
  geom_tile(color = "white")+
  geom_text() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation")

# Combine fixed effects and residual correlations
cormat <- left_join(fixef, rescor, by = c("pred" = "x")) %>%
  pivot_wider(names_from = y, values_from = starts_with("cor_")) %>%
  mutate(across(starts_with("dv_"), ~ifelse(str_detect(pred, "accuracy"), ilogit(.),.)),
         across(where(is.double), round, 2),
         across(starts_with("cor"), numformat),
         across(everything(), replace_na, ""),
         across("dv_est", paste0, " [", dv_lo, " -- ", dv_up,"]"),
         across("cor_est_terminatoraccuracy", paste0, " [", cor_lo_terminatoraccuracy, " -- ", cor_up_terminatoraccuracy, "]"),
         across("cor_est_spacingaccuracy", paste0, " [", cor_lo_spacingaccuracy, " -- ", cor_up_spacingaccuracy, "]"),
         across("cor_est_spellingaccuracy", paste0, " [", cor_lo_spellingaccuracy, " -- ", cor_up_spellingaccuracy, "]"),
         across("cor_est_vocabmeanage", paste0, " [", cor_lo_vocabmeanage, " -- ", cor_up_vocabmeanage, "]"),
         across("cor_est_storygrammar", paste0, " [", cor_lo_storygrammar, " -- ", cor_up_storygrammar, "]"),
         across("cor_est_numberwords", paste0, " [", cor_lo_numberwords, " -- ", cor_up_numberwords,"]"),
         across("cor_est_syntax", paste0, " [", cor_lo_syntax, " -- ", cor_up_syntax,"]"),
         across("cor_est_events", paste0, " [", cor_lo_events, " -- ", cor_up_events,"]"),
         across(everything(), trimws),
         across(everything(), str_remove, "\\[ \\-- \\]")) %>%
  select(-contains("_lo"), -contains("_up")) %>%
  select(pred, starts_with("dv_"), ends_with("_terminatoraccuracy"):ends_with("_events"))

names(cormat) <- str_remove(names(cormat), "cor_est_")         
write_csv(cormat, "rmarkdown/correlationmatrix.csv")
                  
        