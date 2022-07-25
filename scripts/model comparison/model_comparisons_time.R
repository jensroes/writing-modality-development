rm(list=ls());gc()
library(tidyverse)
library(magrittr)
library(brms)

path <- "stanout"
(files <- list.files(path = path, full.names = T, pattern = "mv_null|mv_time_poly|mv_time\\."))
(output_name <- gsub(replacement = "", pattern = paste0(path, "|/|.rda"), files))

for(i in 1:length(files)){assign(output_name[i], add_criterion(readRDS(files[i]), "loo"));gc()}

# Get model names and print
(loos <- ls(pattern = "mv_"))

mc <- do.call("loo", c(lapply(loos, as.name), moment_match = TRUE))
mc$diffs %>% as.data.frame() %>%
  mutate(model=row.names(.)) -> mca;mca

file_out <- paste0("stanout/time_model_comparisons.csv")
write_csv(mca, file_out)

# Redo with increasing model complexity
# Intercept-only vs linear
loo1 <- loo(mv_null, mv_time)
loo1 <- loo1$diff %>% as.data.frame() %>%
  mutate(model = row.names(.),
         comparison = "intercept only vs linear",
         model = recode(model, mv_time = "linear",
                               mv_null = "intercept only"),
         fit = c("baseline", "difference"))

# Linear vs quadratic
loo2 <- loo(mv_null, mv_time_poly2)
loo2 <- loo2$diff %>% as.data.frame() %>%
  mutate(model=row.names(.),
         comparison = "intercept only vs quadratic",
         model = recode(model, mv_null = "intercept only",
                               mv_time_poly2 = "quadratic"),
         fit = c("baseline", "difference"))

# Quadratic vs cubic
loo3 <- loo(mv_time_poly2, mv_time_poly3)
loo3 <- loo3$diff %>% as.data.frame() %>%
  mutate(model=row.names(.),
         comparison = "quadratic vs cubic",
         fit = c("baseline", "difference"),
         model = recode(model, mv_time_poly2 = "quadratic",
                               mv_time_poly3 = "cubic"))

bind_rows(loo1, loo2, loo3) %>%
  as_tibble() %>%
  select(model, comparison, fit, ends_with("_diff"), elpd_loo, se_elpd_loo)
