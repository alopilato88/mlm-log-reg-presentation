#############################################################################
# This script simulates data from a mixed-effects logistic regression model #
#############################################################################

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
    n_cluster = 150,
    odds_ratio_x = exp(logit(.50)),
    odds_ratio_cluster_x = NA,
    odds_ratio_inx = NA,
    ranef_covariance = matrix(1, ncol = 1)
  )

sim_empty_mod_data <- 
  sim_empty_mod_output %>%
  purrr::pluck(
    "data"
  )

# Create simulated multilevel team data for extended example
sim_team_output <-
  glmer_logistic_sim(
    n_ind = 10,
    n_cluster = 150,
    odds_ratio_x = c(exp(logit(.35)),
                     2.50),
    odds_ratio_cluster_x = c(2),
    odds_ratio_inx = 2
  )

# Pluck data from sim_team_output -- think of examples
sim_team_data <-
  sim_team_output %>%
  purrr::pluck(
    "data"
  )
