rm(list=ls());gc()
library(tidyverse)
library(magrittr)
library(bayestestR)
library(brms)

path <- "stanout"
(files <- list.files(path = path, full.names = T, pattern = "spacing_"))
(output_name <- gsub(replacement = "", pattern = paste0(path, "|/|.rda"), files))

for(i in 1:length(files)){assign(output_name[i], 
                                 add_criterion(readRDS(files[i]), "loo"));gc()}

(models <- ls(pattern = "spacing_"))

mc <- do.call("loo", c(lapply(models, as.name), moment_match = TRUE))
mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mca;mca

bf <- do.call("bayesfactor_models", c(lapply(models, as.name), denominator = 1))

mca$bfs <- sort(exp(bf$log_BF),decreasing = T)

file_out <- paste0("stanout/model_comparisons_spacing.csv")
write_csv(mca, file_out)
