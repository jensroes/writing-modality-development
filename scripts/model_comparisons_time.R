rm(list=ls());gc()
library(tidyverse)
library(magrittr)
library(brms)

path <- "stanout"
(files <- list.files(path = path, full.names = T, pattern = "mv_null|mv_time_poly|mv_time\\."))
(output_name <- gsub(replacement = "", pattern = paste0(path, "|/|.rda"), files))

for(i in 1:length(files)){assign(output_name[i], 
                                 add_criterion(readRDS(files[i]), "loo"));gc()}

(loos <- ls(pattern = "mv_"))

mc <- do.call("loo", c(lapply(loos, as.name), moment_match = TRUE))
mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mca;mca

file_out <- paste0("stanout/time_model_comparisons.csv")
write_csv(mca, file_out)
