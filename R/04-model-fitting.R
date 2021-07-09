############################################################################
# This script estimate models from the data created in 03-simulated-data.R #
############################################################################

# --- Fit LMER model for LMER primer section --- #
lmer_lm_m0 <- lm(Y ~ 1, data = lmer_data)
lmer_lm_m1 <- lm(Y ~ X, data = lmer_data)
lmer_lm_m2 <- lm(Y ~ X + as.factor(CLUSTER_ID), data = lmer_data)
lmer_m0 <- lme4::lmer(Y ~ 1 + (1 | CLUSTER_ID), data = lmer_data)
lmer_m1 <- lme4::lmer(Y ~ X + (1 | CLUSTER_ID), data = lmer_data)
lmer_m2 <- lme4::lmer(Y ~ X_CLUSTER_CENT + X_CLUSTER_MEAN + (1 | CLUSTER_ID), 
                      data = lmer_data)

# --- Fit logistic model for logistic primer section --- #
sim_logistic_reg_data <- 
  sim_logistic_reg_data %>%
  dplyr::rename(
    INTENT_TO_LEAVE = X_IND_1,
    JOB_SAT = X_IND_2,
    LEAVE_ORG = Y
  )

logistic_m0 <- glm(LEAVE_ORG ~ 1, 
                   data = sim_logistic_reg_data,
                   family = "binomial")

logistic_m1 <- glm(LEAVE_ORG ~ INTENT_TO_LEAVE + JOB_SAT, 
                   data = sim_logistic_reg_data,
                   family = "binomial")

predict_data <- 
  data.frame(
    OUTCOME = rep(c("Job Satisfaction", "Intet to Leave"), each = 100),
    RESPONSE = rep(seq(-4, 4, len = 100), 2)
  ) %>%
  dplyr::mutate(
    PRED_PROB = dplyr::case_when(
      OUTCOME == "Job Satisfaction" ~ plogis(logistic_m1$coef[1] + logistic_m1$coef[3] * RESPONSE),
      TRUE ~ plogis(logistic_m1$coef[1] + logistic_m1$coef[2] * RESPONSE)
    )
  )

# --- Fit empty GLMER model from GLMER introduction --- #
glmer_m0 <- lme4::glmer(JOB_SAT_BINARY ~ 1 + (1 | FIRM), 
                        data = sim_empty_mod_data, 
                        family = "binomial")

glmer_m1 <- lme4::glmer(JOB_SAT_BINARY ~ SCALED_TENURE + HR_PRACTICES + (1 | FIRM), 
                        data = sim_empty_mod_data, 
                        family = "binomial")

firm_plot_data <- 
  sim_empty_mod_data %>%
  dplyr::group_by(
    FIRM
  ) %>%
  dplyr::summarize(
    FIRM_JOB_SAT = mean(JOB_SAT_BINARY)
  )

uncon_re <- 
  data.frame(
    FIRM = 1:200,
    PROP = plogis(unlist(coef(glmer_m0))),
    COND = "Random Effects"
  ) %>%
  dplyr::bind_rows(
    firm_plot_data %>%
      dplyr::select(
        FIRM,
        PROP = FIRM_JOB_SAT
      ) %>%
      dplyr::mutate(
        COND = "Firm Job Sat. Prop."
      )
  )

conditional_re <- data.frame(
  PROP = plogis(unlist(ranef(glmer_m1)) + summary(glmer_m1)$coef[1, 1]),
  FIRM = 1:200,
  COND = rep("Conditional RE - Avg. Predictor Values", 200)
) %>%
  dplyr::select(
    FIRM,
    PROP,
    COND
  ) %>%
  dplyr::bind_rows(
    data.frame(
      PROP = plogis(unlist(ranef(glmer_m1)) + summary(glmer_m1)$coef[1, 1] + 
        summary(glmer_m1)$coef[2, 1] + summary(glmer_m1)$coef[3, 1]),
      FIRM = 1:200,
      COND = rep("Conditional RE - High Predictor Values", 200)
    )
  ) %>%
  dplyr::bind_rows(
    data.frame(
      PROP = plogis(unlist(ranef(glmer_m1)) + summary(glmer_m1)$coef[1, 1] - 
        summary(glmer_m1)$coef[2, 1] - summary(glmer_m1)$coef[3, 1]),
      FIRM = 1:200,
      COND = rep("Conditional RE - Low Predictor Values", 200)
    )
  ) %>%
  dplyr::bind_rows(
    firm_plot_data %>%
      dplyr::select(
        FIRM,
        PROP = FIRM_JOB_SAT
      ) %>%
      dplyr::mutate(
        COND = rep("Unconditional Prop.", 200)
      )
  )


# --- Fit cross-classified GLMER model for Hiring Recommendation example --- #
glmer_hire_m0 <- lme4::glmer(HIRE_REC ~ 1 + 
                               (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m1 <- lme4::glmer(HIRE_REC ~ ASSESSMENT_1_CAT + ASSESSMENT_2_CAT + 
                               CANDIDATE_GENDER + INTERVIEWER_GENDER + 
                               (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m2 <- lme4::glmer(HIRE_REC ~ ASSESSMENT_1_CAT + ASSESSMENT_2_CAT + 
                               CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                               (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                             data = hiring_data$data_sparse, 
                             family = "binomial")

glmer_hire_m3 <- lme4::lmer(ASSESSMENT_1_CAT ~ CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                              (1 | INTERVIEWER_GENDER) + (1 | CANDIDATE_GENDER), 
                            data = hiring_data$data_sparse)

glmer_hire_m4 <- lme4::lmer(ASSESSMENT_2_CAT ~ CANDIDATE_GENDER * INTERVIEWER_GENDER + 
                              (1 | INTERVIEWER_GENDER) + (1 | CANDIDATE_GENDER), 
                            data = hiring_data$data_sparse)

glmer_final <- lme4::glmer(HIRE_REC ~ ASSESSMENT_1_CAT + ASSESSMENT_2_CAT + 
                             (1 | INTERVIEWER_ID) + (1 | CANDIDATE_ID), 
                           data = hiring_data$data_sparse, 
                           family = "binomial")

# Chances of hiring rec change based on random effect 
summ_coef <- summary(glmer_final)$coef

linear_pred <- summ_coef[1, 1] + summ_coef[2, 1] * hiring_data$data_sparse$ASSESSMENT_1_CAT + 
  summ_coef[3, 1] * hiring_data$data_sparse$ASSESSMENT_2_CAT

n_row <- nrow(hiring_data$data_sparse)

hire_re_plot_data <- 
  tibble::tibble(
    PROP = c(
      plogis(linear_pred + 0),
      plogis(linear_pred + 1),
      plogis(linear_pred - 1)
    ),
    COND = c(
      rep("Average Interviewer", n_row),
      rep("Lenient Interviewer", n_row),
      rep("Harsh Interviewer", n_row)
    )
  )

# Assessment 1 Plots 
plot_1_data <- 
  tibble::tibble(
    ASSESSMENT_1 = 0:3,
    PROP = plogis(
      summ_coef[1, 1] + summ_coef[3, 1] * 2 + summ_coef[2, 1] * 0:3
    )
  )

plot_2_data <- 
  tibble::tibble(
    ASSESSMENT_1 = 0:3,
    PROP = plogis(
      summ_coef[1, 1] + summ_coef[3, 1] * 2 + summ_coef[2, 1] * 0:3 + 1
    )
  )

plot_3_data <- 
  tibble::tibble(
    ASSESSMENT_1 = 0:3,
    PROP = plogis(
      summ_coef[1, 1] + summ_coef[3, 1] * 2 + summ_coef[2, 1] * 0:3 + -1
    )
  )

plot_4_data <- 
  tibble::tibble(
    RE = (unlist(ranef(glmer_final)[["INTERVIEWER_ID"]]))
  )



  