#############################################################################
# This script simulates data from a mixed-effects logistic regression model #
#############################################################################

# --- Create LMER model example --- #
lmer_data <- lmer_sim(n1 = 100, n2 = 20)

# --- Create logistic regression example --- #
sim_logistic_reg_output <-
  glmer_logistic_sim(
    n_ind = 1000,
    n_cluster = 2,
    odds_ratio_x = c(exp(logit(.35)),
                     10,
                     .25),
    odds_ratio_cluster_x = NA,
    odds_ratio_inx = NA,
    ranef_covariance = matrix(0, ncol = 1)
  )

sim_logistic_reg_data <-
  sim_logistic_reg_output %>%
  purrr::pluck(
    "data"
  )

# --- Create random effects model --- #
sim_empty_mod_output <- 
  glmer_logistic_sim(
    n_ind = 100,
    n_cluster = 200,
    odds_ratio_x = c(exp(0), exp(1)),
    odds_ratio_cluster_x = exp(4.20),
    odds_ratio_inx = NA,
    ranef_covariance = matrix(3, ncol = 1)
  )

sim_empty_mod_data <- 
  sim_empty_mod_output %>%
  purrr::pluck(
    "data"
  ) %>%
  dplyr::select(
    JOB_SAT_BINARY = Y,
    SCALED_TENURE = X_IND_1,
    HR_PRACTICES = X_CLUSTER_1,
    FIRM = CLUSTER_ID
  ) %>%
  dplyr::mutate(
    JOB_SAT_TEXT = dplyr::case_when(
      JOB_SAT_BINARY == 1 ~ "Satisfied",
      TRUE ~ "Not Satisfied"
    )
  )

# --- Create cross-classified hiring recommendation simulated data -- #
hiring_data <- 
  hiring_recomendation_sim(
    n_candidates = 1000,
    n_interviewers = 300,
    sparse_data = TRUE
  ) 

