## Setup ----

library(tidyverse)
library(magrittr)
library(readxl)
library(stringdist)
library(tokenizers)

Df <- read_excel("data/Transcriptions and coding study 2 and 3.xlsx",
                 sheet = "all texts with all keys")



### Length: number of words ----


## tokenise trans1.5 and count words


Df %<>% 
  # rowwise needed otherwise length gets the length of the column, not the
  # length of what's in a particular row of the column
  rowwise() %>% 
  mutate(trans1.5_word_count = length(unlist(tokenize_words(trans1.5_spacing_corrected)))) %>% 
  ungroup() # this removes effect of rowwise


## count all NW and subtract these from word_count
Df %<>% mutate(trans1.5_nonword_count = str_count(trans1.5_spacing_corrected,'\\[NW')
               ,trans1.5_word_count = trans1.5_word_count - 2*trans1.5_nonword_count)




###Space use accuracy ----

temp1 <- Df %>% 
  mutate(trans1_spaces = str_count(trans1, ' ') #spaces used by student
         ,trans1.5_spaces = str_count(trans1.5_spacing_corrected, ' ') #spaces that should be there, but including within NW
         ,spaces_NW = str_count(trans1.5_spacing_corrected, '[N]') #count number of spaces in NW
         ,trans1.5_adjusted = (trans1.5_spaces - spaces_NW) #number of spaces that should be there
         ,og = ifelse(is.na(og), 0, og)
         ,spaces_missing = trans1.5_adjusted - (trans1_spaces - og)
         ,correct_spaces = trans1_spaces - og) %>% 
  select(text_id, correct_spaces, 
                  additional_spaces = og, 
                  missing_spaces = spaces_missing)
# JR: I'll will model the space accuracy, from correct, missing and incorrect spaces

Df %<>% left_join(temp1, by = 'text_id')

rm(temp1)


### Sentence terminators ----


## count number of sentence terminators (do the students use this or not)? 
## how many of these should be there? 
## how many terminators are missing?
## if the student uses terminators, how many of the sentences are corretly terminated (propotion)?
## the final measure, correct_terminators means that 1 stands for more correct terminators than wrong, 
## while 0 means more wrong than correct terminators or as many wrong as correct or no use of terminators at all

temp2 <- Df %>% 
  mutate(terminators_used = str_count(trans2_spelling_corrected, '[.!?:]'), #number they have used
         terminators_correct = str_count(trans3_wrong_punctuation_removed, '[.!?:]'), #number correctly put in by student
         terminators_required = str_count(`trans3.5_missing_ punctuation_inserted`, '[.!?:]'), #number that should be there
         terminators_missing = terminators_required - terminators_correct,
         additional_terminators = terminators_used - terminators_correct) %>%
  select(text_id,
         additional_terminators,
         correct_terminators = terminators_correct,
         missing_terminators = terminators_missing)

temp2 %>% filter(additional_terminators < 0)
#temp2 %>% filter(missing_terminators < 0)

# JR: I'll will model the terminator accuracy, from correct, missing and incorrect terminators

Df %<>% left_join(temp2, by = 'text_id')

rm(temp2)

### Spelling ----


#'tokenize trans1.5 and 2 then for pairs of words find lev distance, 
# and then proportion of incorrect words.'

Df2 <- Df %>% 
  select(text_id ,trans1.5_spacing_corrected, trans2_spelling_corrected) %>% 
  mutate(trans1.5_real_words = str_replace_all(trans1.5_spacing_corrected, ' *\\[NW [a-zæøåòó§0-9,.!?: ]+\\]', ''),
         trans2_real_words = str_replace_all(trans2_spelling_corrected, ' *\\[NW [a-zæøåòó§0-9,.!?: ]+\\]', ''),
         t1.5_wds = tokenize_words(trans1.5_real_words),
         t2_wds = tokenize_words(trans2_real_words)) %>% 
  rowwise() %>% 
  mutate(diff = length(unlist(t1.5_wds)) - length(unlist(t2_wds))) 


# L1 and L2 are equal-length lists of words, that are then
# compared pairwise

Df2$levs <- pmap(list(Df2$t1.5_wds,Df2$t2_wds),stringdist, method = 'lv')

Df2$mean_Lev <- unlist(map(Df2$levs, mean))

Df2$mistakes <- map(Df2$levs, function(x){ifelse(x == 0, 0, 1)})

Df2$correct_spelling <- map(Df2$levs, function(x){ifelse(x == 0, 1, 0)})

Df2$correct_spelling <- unlist(map(Df2$correct_spelling, sum))

Df2$spell_prpn_error <- unlist(map(Df2$mistakes, sum))/unlist(map(Df2$levs,length))

Df2 %<>% select(text_id, mean_Lev, correct_spelling, spell_prpn_error)

Df %<>% left_join(Df2, by = 'text_id')

rm(Df2)

### Vocabulary ----
library(psych)


Df2 <- Df %>% 
  select(text_id, trans2.5_vocabulary) %>% 
  mutate(t2.5_wds = tokenize_words(trans2.5_vocabulary))

# get types per text (i.e. remove repeated words) 
Df2$t2.5_wds = map(Df2$t2.5_wds,unique)

Df2 %<>% unnest(t2.5_wds)

# get ratings ----
# ratings3 is from the survey used in study 1 (21 raters), ratings1 and ratings2 
# are new for this study with 16 unique raters on each
Df_ratings1 <- read_excel("data/Study_2_vocabulary_ratings.xlsx", 
                         sheet = "ratings1")

Df_ratings2 <- read_excel("data/Study_2_vocabulary_ratings.xlsx", 
                         sheet = "ratings2")

Df_ratings3 <- read_excel("data/Study_2_vocabulary_ratings.xlsx", 
                         sheet = "ratings3")

#for matching types in text to the lemmas that were rated
Df_text2rated <- read_excel("data/Study_2_vocabulary_ratings.xlsx", 
                            sheet = "wds_intext2rated_complete")

## agreement and descriptives

# # overall inter-rater agreement (Cronbach's alpha)
Df_ratings1 %>% 
  select('1':'16') %>% 
  alpha(.)

Df_ratings2 %>% 
  select('1':'16') %>% 
  alpha(.)

Df_ratings3 %>% 
  select('1':'21') %>% 
  alpha(.)

#get mean rating across rater for each lemma
Df_ratings1 %<>% 
  mutate(age_rating = rowMeans(select(.,-word_as_rated))) %>% 
  select(word_as_rated,age_rating)

Df_ratings2 %<>% 
  mutate(age_rating = rowMeans(select(.,-word_as_rated)))%>% 
  select(word_as_rated,age_rating)

Df_ratings3 %<>% 
  mutate(age_rating = rowMeans(select(.,-word_as_rated)))%>% 
  select(word_as_rated,age_rating)

Df_ratings1 %<>%  rbind(Df_ratings2, Df_ratings3)


# upper 30th centile boundary
pc70 = quantile(Df_ratings1$age_rating, probs = c(.7))

Df_ratings1 %<>% 
  mutate(upper_30 = ifelse(age_rating >= pc70 , 1, 0))

# merge with words in text to create word-in-text to rating lookup table
word2rating <- left_join(Df_text2rated
                         ,Df_ratings1 %>% select(word_as_rated,age_rating, upper_30)
                         , by = "word_as_rated")


Df2 %<>% left_join(word2rating %>% select(-word_as_rated)
                   , by = c("t2.5_wds" = "word_in_text"))

Df3 <- Df2 %>% 
  group_by(text_id) %>% 
  summarise(vocab_mean_age = mean(age_rating, na.rm = T)
            , vocab_count_advanced = sum(upper_30, na.rm = T))



#median split, or divide in groups 0 vs. 1 or more
Df3 %<>% 
mutate(voc_median_split = ntile(vocab_mean_age, 2))

Df3 %<>% mutate(voc_split_oneorzero = ifelse(vocab_count_advanced > 0, 1, 0))

Df %<>% left_join(Df3, by = 'text_id')

rm(Df_text2rated, Df2, Df3, word2rating, pc70, Df_ratings1, Df_ratings2, Df_ratings3)



### Syntactic complexity ----

## count number of correct and incorrect main clauses and correct
## and incorrect subordinated clauses to find the total syntactic score

temp3 <- Df
temp3 %<>% mutate(mainc_corr = str_count(trans4_syntax, 'M |MP ')
                  ,mainc_incorr = str_count(trans4_syntax, 'MW|MWP')
                  ,subc_corr = str_count(trans4_syntax, 'S |SP|SQ |SQP')
                  ,subc_incorr = str_count(trans4_syntax, 'SW|SWP|SQW')
                  ,sentences_count = mainc_corr + mainc_incorr + subc_corr + subc_incorr)


temp3 %<>% mutate(score_mc = mainc_corr * 1
                  ,score_sc = subc_corr * 2
                  ,score_mic = mainc_incorr * 0.5
                  ,score_sic = subc_incorr * 1
                  ,advanced_syntax = score_mc + score_sc + score_mic + score_sic
                  ,advanced_syntax_dich = ifelse(advanced_syntax >= 2, 1, 0))


temp3 %<>% select(text_id, advanced_syntax, advanced_syntax_dich)
Df %<>% left_join(temp3, by = 'text_id')

rm(temp3)

### Narrative structure ----

# find story grammar

temp4 <- Df %>% 
  rowwise()

temp4 %<>% 
  mutate(orientation = str_count(trans5_narrative, 'O'),
         complication = str_count(trans5_narrative, 'C'),
         resolution = str_count(trans5_narrative, 'R'),
         story_grammar = sum(orientation + complication + resolution),
         story_grammar = ifelse(story_grammar <= 1,0,story_grammar - 1))
  
#story grammar dichotomized
temp4 %<>% 
  mutate(story_grammar_dich  = ifelse(story_grammar >= 1,1,0))



# find events
temp4 %<>% 
  mutate(events = str_count(trans5_narrative, 'E'),
         events_dich = ifelse(events < 1, 0, 1))



# find advanced structures
temp4 %<>%
  mutate(problems = str_count(trans5_narrative, 'P'),
         solutions = str_count(trans5_narrative, 'S'),
         effects = str_count(trans5_narrative, 'F'),
         reactions = str_count(trans5_narrative, 'A'),
         narrator = str_count(trans5_narrative, 'N'),
         advanced_structures = sum(problems + solutions + effects + 
                                     reactions + narrator),
         advanced_structures_dich = ifelse(advanced_structures >= 2, 1, 0))


temp4 %<>% select(text_id, story_grammar,story_grammar_dich, events, events_dich, advanced_structures, 
                  advanced_structures_dich) %>% 
  ungroup()

Df %<>% left_join(temp4, by = 'text_id')
rm(temp4)

DV <- Df %>% 
  select(text_id, student = student_id, school, condition, timepoint, number_words = trans1.5_word_count,
         number_nonwords = trans1.5_nonword_count, contains("_spaces"), contains("_terminators"), 
         spelling_correct = correct_spelling, 
         vocab_mean_age, vocab_count_advanced, syntax=advanced_syntax, advanced_syntax_dich, story_grammar, events, 
         events_dich, advanced_structures, advanced_structures_dich)

DV <- DV %>% 
   select(text_id, student, school, condition, timepoint, number_words, spelling_correct, vocab_mean_age, syntax, story_grammar, events, 
          advanced_structures,  contains("_spaces"), contains("_terminators"))

# Remove everything, including packages, apart from DV
rm(list=Filter(function(x) {!(x %in% 'DV')}, ls()))


#Finding age in months (all DigiHandstudents)

library(readxl)
Age <- read_excel("data/Parentsquest with birth date05.11.xlsx") %>%
  mutate(age = ((2018 - SP_Year)*12)-SP_Month + 9
         ,age = ifelse(is.na(age), (2018 - 2012)*12 - 6 + 9, age)) %>% 
  select(student = ElevID, age)


DV %<>% left_join(Age, by = "student", copy = FALSE) %>% 
  mutate(condition = factor(condition
                            ,levels = c('x','y')
                            ,labels = c('handwriting','typing')),
         across(c(student,school), factor)) %>%
  rename(modality = condition,
         time = timepoint,
         child = student)
rm(Age)


## --- Add covariates

covars <- readxl::read_xlsx("data/teacher quest.xlsx", sheet = 2) %>%
  select(teacher_id, school, modality = condition, starts_with("T_TJ")) %>%
  mutate(school = as.character(school))

DVcovars <- DV %>% left_join(covars, by = c("modality", "school")) %>% 
  rename(write_story = T_TJTextStory,
         discuss_text = T_TJDiscussText,
         discuss_structure = T_TJStruct) %>%
  mutate(across(c(write_story, discuss_text, discuss_structure, time), ~.-min(.)))


saveRDS(DVcovars, file = "data/modality_development.Rdata", compress = "xz")




