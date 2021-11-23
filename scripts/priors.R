# Priors
prior_mod <- set_prior("student_t(2, 0, 2)", class = "b", coef = "modalitytyping",
                       resp = c("advancedstructures", "events", "syntax")) +
  set_prior("student_t(2, 0, 2)", class = "b", resp = "numberwords", coef = "modalitytyping") +
  set_prior("student_t(2, 0, 2)", class = "b", resp = "storygrammar", coef = "modalitytyping") +
  set_prior("student_t(2, 0, 2)", class = "b", resp = "vocabmeanage", coef = "modalitytyping",) +
  set_prior("student_t(2, 0, 2)", class = "b", 
            resp = c("spellingcorrect", "correctspaces", "correctterminators"),
            coef = "modalitytyping")

dvs <- c("advancedstructures", "events", "syntax", "numberwords", "storygrammar", "vocabmeanage",
         "spellingcorrect", "correctspaces", "correctterminators")

prior_poly1 <- set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime1", resp = dvs) 

prior_poly21 <- set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime21", resp = dvs) 

prior_poly22 <- set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime22", resp = dvs) 

prior_poly31 <- set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime31", resp = dvs) 

prior_poly32 <- set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime32", resp = dvs)

prior_poly33 <- set_prior("student_t(5, 0, 10)", class = "b", coef = "polytime33", resp = dvs) 

prior_poly21mod <- set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime21:modalitytyping", resp = dvs) 

prior_poly22mod <- set_prior("student_t(5, 0, 5)", class = "b", coef = "polytime22:modalitytyping", resp = dvs) 

prior_covar <- set_prior("student_t(5, 0, 5)", class = "b", coef = c("discuss_text", "discuss_structure", "write_story"), resp = dvs)

prior_covar2way <- set_prior("student_t(5, 0, 5)", class = "b", coef = c("polytime22:write_story",
                                                                         "polytime22:discuss_text", 
                                                                         "polytime22:discuss_structure",
                                                                         "polytime21:write_story", 
                                                                         "polytime21:discuss_text", 
                                                                         "polytime21:discuss_structure",
                                                                         "modalitytyping:write_story", 
                                                                         "modalitytyping:discuss_text",
                                                                         "modalitytyping:discuss_structure"), resp = dvs)

prior_covar3way <- set_prior("student_t(5, 0, 5)", class = "b", coef = c("polytime21:modalitytyping:discuss_structure",
                                                                         "polytime21:modalitytyping:discuss_text", 
                                                                         "polytime21:modalitytyping:write_story"
#                                                                         "polytime22:modalitytyping:discuss_structure",
 #                                                                        "polytime22:modalitytyping:discuss_text", 
  #                                                                       "polytime22:modalitytyping:write_story"
), resp = dvs)


#prior <- set_prior("normal(0, 2)", class = "Intercept", 
#                   resp = c( "advancedstructures", "events", "syntax")) +
#  set_prior("normal(0, 1)", class = "b", 
#            resp = c( "advancedstructures", "events", "syntax")) +
#  set_prior("normal(2, 2)", class = "Intercept", resp = "numberwords") +
#  set_prior("normal(0, 1)", class = "b", resp = "numberwords") +
#  set_prior("normal(1, 4)", class = "Intercept", resp = "storygrammar") +
#  set_prior("normal(0, 1)", class = "b", resp = "storygrammar") +
#  set_prior("normal(7, 1)", class = "Intercept", resp = "vocabmeanage") +
#  set_prior("normal(0, 1)", class = "b", resp = "vocabmeanage") +
#  set_prior("normal(0, 3)", class = "Intercept", 
#            resp = c("spellingcorrect", "correctspaces", "correctterminators")) +
#  set_prior("normal(0, 1)", class = "b", 
#            resp = c("spellingcorrect", "correctspaces", "correctterminators"))

