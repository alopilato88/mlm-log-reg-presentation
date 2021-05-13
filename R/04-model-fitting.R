############################################################################
# This script estimate models from the data created in 03-simulated-data.R #
############################################################################

# --- Fit LMER model for LMER primer section --- #
lmer_lm_m0 <- lm(Y ~ 1, data = lmer_data)
lmer_lm_m1 <- lm(Y ~ X, data = lmer_data)
lmer_m0 <- lme4::lmer(Y ~ 1 + (1 | CLUSTER_ID), data = lmer_data)
lmer_m1 <- lme4::lmer(Y ~ X + (1 | CLUSTER_ID), data = lmer_data)
lmer_m2 <- lme4::lmer(Y ~ X_CLUSTER_CENT + X_CLUSTER_MEAN + (1 | CLUSTER_ID), 
                      data = lmer_data)

# --- Fit logistic model for logistic primer section --- #
logistic_m0 <- glm(Y ~ 1, 
                   data = sim_logistic_reg_data,
                   family = "binomial")

logistic_m1 <- glm(Y ~ X_IND_1 + X_IND_2, 
                   data = sim_logistic_reg_data,
                   family = "binomial")

# --- Fit empty GLMER model from GLMER introduction --- #
glmer_m0 <- lme4::glmer(Y ~ 1 + (1 | CLUSTER_ID), 
                        data = sim_empty_mod_data, 
                        family = "binomial")

# --- Fit cross-classified GLMER model for Hiring Recommendation example --- #
glmer_hire_m0 <- lme4::glmer(HIRE_REC ~ 1 + 
                               (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m1 <- lme4::glmer(HIRE_REC ~ CANDIDATE_GENDER + INTERVIEWER_GENDER + 
                               (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m2 <- lme4::glmer(HIRE_REC ~ CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                               (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m3 <- lme4::glmer(HIRE_REC ~ CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                               ASSESSMENT_1_CAT + ASSESSMENT_2_CAT + 
                               (1 | CANDIDATE_ID) + (1 | INTERVIEWER_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m4 <- lme4::lmer(ASSESSMENT_1_CAT ~ CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                              (1 | INTERVIEWER_GENDER) + (1 | CANDIDATE_GENDER), 
                            data = hiring_data$data_sparse)

glmer_hire_m5 <- lme4::lmer(ASSESSMENT_2_CAT ~ CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                              (1 | INTERVIEWER_GENDER) + (1 | CANDIDATE_GENDER), 
                            data = hiring_data$data_sparse)

