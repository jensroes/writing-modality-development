rm(list=ls());gc()
library(tidyverse)
library(magrittr)
library(bayestestR)
library(brms)

path <- "stanout"
(files <- list.files(path = path, full.names = T, pattern = "mv_terminators"))
(output_name <- gsub(replacement = "", pattern = paste0(path, "|/|.rda"), files))

for(i in 1:length(files)){assign(output_name[i], 
                                 add_criterion(readRDS(files[i]), "loo"));gc()}

(models <- ls(pattern = "terminators_"))

mc <- do.call("loo", c(lapply(models, as.name), moment_match = TRUE))
mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mca;mca

file_out <- paste0("stanout/model_comparisons_terminators.csv")
write_csv(mca, file_out)
