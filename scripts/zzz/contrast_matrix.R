library(tidyverse)

# Data
modality <- readRDS(file = "data/modality_development.Rdata") %>%
  mutate(story_grammar = ordered(story_grammar),
         spacing_total = correct_spaces + additional_spaces + missing_spaces,
         terminators_total = correct_terminators + additional_terminators + missing_terminators,
         across(c(time, modality), factor),
         across(c(advanced_structures, events, syntax, number_words, 
                  spelling_correct, correct_spaces, 
                  spacing_total, terminators_total), as.integer)) 

modality %>% filter_all(any_vars(is.na(.))) %>%
  glimpse()

modality <- modality %>% drop_na()

# Determine contrasts
modality$COND <- paste(modality$modality, modality$time, sep = "_")
modality$COND <- as.factor(modality$COND)
summary(modality$COND)

cmat <- MASS::fractions(matrix(c(
  # Main effects
  # Sum coding on adjacency
   1, 1, 1, 1, 1,-1,-1,-1,-1,-1, # modality
   
  -1, 1, 0, 0, 0,-1, 1, 0, 0, 0, # time 0-1
   0,-1, 1, 0, 0, 0,-1, 1, 0, 0, # time 1-2
   0, 0,-1, 1, 0, 0, 0,-1, 1, 0, # time 2-3
   0, 0, 0,-1, 1, 0, 0, 0,-1, 1, # time 3-4

  -1, 1, 0, 0, 0, 1,-1, 0, 0, 0, # time 1 * modality
   0,-1, 1, 0, 0, 0, 1,-1, 0, 0, # time 2 * modality
   0, 0,-1, 1, 0, 0, 0, 1,-1, 0, # time 3 * modality
   0, 0, 0,-1, 1, 0, 0, 0, 1,-1 # time 4 * modality

), nrow=length(levels(modality$COND)), byrow=F))

rown <- levels(modality$COND)
rownames(cmat) <- rown

mod <- c("Modality") 
time <- paste("Time", 1:4)
coln <- c(mod, time)
for(i in time){
  coln <- c(coln, paste(mod, i, sep = " x "))
}
colnames(cmat)<- coln
cmat; colSums(cmat)

# transpose and inverse matrix to get contrast between the expected levels
inv.cmat <- t(MASS::ginv(cmat))
rownames(inv.cmat) <- rown
colnames(inv.cmat) <- coln
#inv.cmat

# Assign contrasts