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

# --- Fit cross-classified GLMER model for Hiring Recomendation example --- #