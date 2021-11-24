rm(list=ls());gc()
library(tidyverse)
library(magrittr)
library(brms)

path <- "stanout"
(files <- list.files(path = path, full.names = T, pattern = ".rda", ))
files <- files[grepl(pattern = "mv_", x = files)]
(files <- files[!grepl(pattern = "terminators|classroom", x = files)])
(output_name <- gsub(replacement = "", pattern = paste0(path, "|/|.rda"), files))

for(i in 1:length(files)){assign(output_name[i], add_criterion(readRDS(files[i]), "loo"));gc()}

(loos <- ls(pattern = "mv_"))

mc_1 <- loo(mv_null, mv_time, mv_time_poly2, mv_time_modality_poly2, mv_time_x_modality_poly2, moment_match = TRUE)
mc_2 <- loo(mv_time_x_modality_poly2, mv_time_x_modality_covars_poly2, mv_time_x_modality_x_covars_poly2, moment_match = TRUE)

mc_1$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mc_1a;mc_1a

mc_2$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mc_2a;mc_2a

#mc <- do.call("loo", c(lapply(loos, as.name), moment_match = TRUE))
mc <- loo(mv_time_poly2, mv_time_modality_poly2, mv_time_x_modality_poly2, 
          mv_time_x_modality_covars_poly2, mv_time_x_modality_x_covars_poly2, moment_match = TRUE)
mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mca;mca

file_out <- paste0("stanout/model_comparison_nocovars.csv")
write_csv(mc_1a, file_out)

file_out <- paste0("stanout/model_comparison_covars.csv")
write_csv(mc_2a, file_out)

file_out <- paste0("stanout/model_comparison_allmodels.csv")
write_csv(mca, file_out)
