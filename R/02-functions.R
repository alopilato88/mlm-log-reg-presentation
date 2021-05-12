###########################################################################
# This script contains all of the user-defined functions used throughout  #
# the remaining scripts.                                                  #
###########################################################################

# logit function to turn probability value into log odds ratio
logit <- function(x) {

  lx <- log(x / (1 - x))
  return(lx)

}

# inverse logit function to turn log odds ratio to probability
inv_logit <- function(x) {

  p <- exp(x) / (1 + exp(x))
  return(p)

}

# glmer_logistic_sim function simulates data from a mixed-effects logistic
# regression model
glmer_logistic_sim <- function(
  n_ind = 100,
  n_cluster = 100,
  odds_ratio_x = c(.50, 1.50),
  odds_ratio_cluster_x = c(1, .25),
  odds_ratio_inx = c(1),
  ranef_covariance = matrix(c(.80, .1 * sqrt(.8) * sqrt(.3),
                              .1 * sqrt(.8) * sqrt(.3), .30),
                            byrow = TRUE,
                            nrow = 2, ncol = 2)
) {

  # total sample size
  n <- n_ind * n_cluster

  # Determine number of random effects
  num_ranef <- ncol(ranef_covariance)

  # Create group id
  cluster_id <- rep(1:n_cluster, each = n_ind)

  # Create random effects design matrix, Z
  Z <- model.matrix(~ as.factor(cluster_id) - 1)

  # Generate random effects
  ranef_matrix <- mvtnorm::rmvnorm(
    n = n_cluster,
    mean = rep(0, num_ranef),
    sigma = ranef_covariance
  )

  ranef <-
    ranef_matrix %>%
    matrix(ncol = 1)

  # Create design matrix
  X <- rep(1, n)

  # Add independent individual level predictors if there are any
  if(length(odds_ratio_x) > 1) {
    x_var <- mvtnorm::rmvnorm(
      n = n,
      mean = rep(0, (length(odds_ratio_x) - 1)),
      sigma = diag(length(odds_ratio_x) - 1)
    )

    X <- cbind(X, x_var)
  }

  # Add independent group level predictors if there are any
  if(sum(!is.na(odds_ratio_cluster_x)) > 0) {
    cluster_x_var <- mvtnorm::rmvnorm(
      n = n_cluster,
      mean = rep(0, length(odds_ratio_cluster_x)),
      sigma = diag(length(odds_ratio_cluster_x))
    ) %>%
      apply(., 2, function(x) rep(x, each = n_ind))

    X <- cbind(X, cluster_x_var)
  }

  if(num_ranef > 1) {
   for(i in 1:(num_ranef - 1)){
     Z0 <- apply(Z, 2, function(x) x * x_var[, i])
     Z <- cbind(Z, Z0)
     X <- cbind(X, cluster_x_var[, i] * x_var[, i])
   }

  }

  # Create log odds ratio matrices
  B <-
    odds_ratio_x %>%
    log() %>%
    as.matrix(ncol = 1)

  if(sum(!is.na(odds_ratio_cluster_x)) > 0) {
    Bg <-
      odds_ratio_cluster_x %>%
      log() %>%
      as.matrix(ncol = 1)

    B <- rbind(B, Bg)
  }

  if(sum(!is.na(odds_ratio_inx)) > 0) {
    Binx <-
      odds_ratio_inx %>%
      log() %>%
      as.matrix(ncol = 1)

    B <- rbind(B, Binx)
  }

  # Mean function
  mu <- X %*% B + Z %*% ranef

  # Use inverse logit function to transform mu into probabilities
  mu_prob <- inv_logit(mu)

  # Generate Y from binomial distribution using mu_prob
  Y <- rbinom(n, 1, prob = mu_prob)

  # Create a data frame to return
  data <- data.frame(
    X,
    Y,
    CLUSTER_ID = cluster_id
  )

  # Name data to make it easier to determine variables
  if(length(odds_ratio_x) > 1) {
    x_ind_names <- paste0("X_IND_", 1:(length(odds_ratio_x) - 1))
  } else {
    x_ind_names <- NA
  }

  if(sum(!is.na(odds_ratio_cluster_x)) > 0) {
    x_cluster_names <- paste0("X_CLUSTER_", 1:length(odds_ratio_cluster_x))
  } else {
    x_cluster_names <- NA
  }

  if(sum(!is.na(odds_ratio_inx)) > 0) {
    x_inx_names <- paste0("X_INX_", 1:length(odds_ratio_cluster_x))
  } else {
    x_inx_names <- NA
  }

  data_names <- c("INTERCEPT", x_ind_names, x_cluster_names, x_inx_names)

  if(sum(!is.na(data_names)) > 0) {
    data_names <- na.omit(data_names)
  }

  names(data)[1:length(data_names)] <- data_names

  data <-
    data %>%
    tibble::as_tibble()

  # Return list that contains all output
  output <- list(
    data = data,
    Z = Z,
    ranef = ranef_matrix
  )

  return(output)
}

# hiring_recomendation_sim function to create cross-classified hiring rec data 
hiring_recomendation_sim <- function(
  n_candidates = 100,
  n_interviewers = 100,
  sparse_data = FALSE
) {
  # Calculate total sample
  n <- n_candidates * n_interviewers
  
  # Create interviewer data frame
  interviewer_data <- 
    tibble::tibble(
      INTERVIEWER_ID = 1:n_interviewers,
      INTERVIEWER_GENDER = sample(c(1, 0), 
                                  size = n_interviewers,
                                  prob = c(.60, .40),
                                  replace = TRUE)
    )
  
  # Create candidate data frame
  candidate_data <- 
    tibble::tibble(
      CANDIDATE_ID = 1:n_candidates,
      CANDIDATE_GENDER = sample(c(1, 0), 
                                size = n_candidates,
                                prob = c(.60, .40), 
                                replace = TRUE)
    )
  
  # Link interviewers and candidates 
  interview_data <- 
    expand.grid(interviewer_data$INTERVIEWER_ID,
                candidate_data$CANDIDATE_ID) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(
      INTERVIEWER_ID = Var1,
      CANDIDATE_ID = Var2
    ) %>%
    dplyr::left_join(
      interviewer_data,
      by = "INTERVIEWER_ID"
    ) %>%
    dplyr::left_join(
      candidate_data,
      by = "CANDIDATE_ID"
    )
  
  # Set model parameters - Selection 1 - Male Bias
  b_intercept_1 <- 2
  b_can_gender_1 <- 0
  b_int_gender_1 <- -.75
  b_int_can_gender_1 <- 1.75 
  
  # Set model parameters - Selection 2 - Female Bias
  b_intercept_2 <- 2
  b_can_gender_2 <- -1
  b_int_gender_2 <- -.75
  b_int_can_gender_2 <- 1
  
  # Set model parameters - Hiring Recommendation 
  b_intercept_3 <- -5
  b_s1_3 <- 1.25
  b_s2_3 <- .75
  
  # Create design matrices 
  Zcandidate <- model.matrix(~ as.factor(interview_data$CANDIDATE_ID)  - 1)
  Zinterviewer <- model.matrix(~ as.factor(interview_data$INTERVIEWER_ID)  - 1)
  Z <- cbind(Zcandidate, Zinterviewer)
  
  # Generate random effects for different models 
  candidate_ranef_1 <- rnorm(n_candidates, mean = 0, sd = 1)
  interviewier_ranef_1 <- rnorm(n_interviewers, mean = 0, sd = 1)
  ranef_1 <- c(candidate_ranef_1, interviewier_ranef_1)
  
  candidate_ranef_2 <- rnorm(n_candidates, mean = 0, sd = 1)
  interviewier_ranef_2 <- rnorm(n_interviewers, mean = 0, sd = 1)
  ranef_2 <- c(candidate_ranef_2, interviewier_ranef_2)
  
  candidate_ranef_3 <- rnorm(n_candidates, mean = 0, sd = sqrt(2))
  interviewier_ranef_3 <- rnorm(n_interviewers, mean = 0, sd = sqrt(2))
  ranef_3 <- c(candidate_ranef_3, interviewier_ranef_3)
  
  # Generate random interview level error term 
  e1 <- rnorm(n, mean = 0, sd = 1)
  e2 <- rnorm(n, mean = 0, sd = 1)
  
  # Generate outcome 
  y_s1 <- b_intercept_1 + b_can_gender_1 * interview_data$CANDIDATE_GENDER + 
    b_int_gender_1 * interview_data$INTERVIEWER_GENDER + 
    b_int_can_gender_1 * interview_data$INTERVIEWER_GENDER * interview_data$CANDIDATE_GENDER + 
    Z %*% ranef_1 + e1 
  
  y_s2 <- b_intercept_2 + b_can_gender_2 * interview_data$CANDIDATE_GENDER + 
    b_int_gender_2 * interview_data$INTERVIEWER_GENDER + 
    b_int_can_gender_2 * interview_data$INTERVIEWER_GENDER * interview_data$CANDIDATE_GENDER + 
    Z %*% ranef_2 + e2
  
  # Categorize y_s1 and y_s2 into buckets of 0 - 3
  interview_data <- 
    interview_data %>%
    dplyr::mutate(
      ASSESSMENT_1 = as.numeric(y_s1), 
      ASSESSMENT_2 = as.numeric(y_s2),
    ) %>%
    dplyr::mutate(
      ASSESSMENT_1_CAT = dplyr::case_when(
        ASSESSMENT_1 <= quantile(ASSESSMENT_1, .25) ~ 0,
        ASSESSMENT_1 <= quantile(ASSESSMENT_1, .50) ~ 1,
        ASSESSMENT_1 <= quantile(ASSESSMENT_1, .75) ~ 2,
        TRUE ~ 3
      ),
      ASSESSMENT_2_CAT = dplyr::case_when(
        ASSESSMENT_2 <= quantile(ASSESSMENT_2, .25) ~ 0,
        ASSESSMENT_2 <= quantile(ASSESSMENT_2, .50) ~ 1,
        ASSESSMENT_2 <= quantile(ASSESSMENT_2, .75) ~ 2,
        TRUE ~ 3
      )
    )
  
  # Generate hiring recommendation
  
  mu_s3 <- b_intercept_3 + b_s1_3 * interview_data$ASSESSMENT_1_CAT + 
    b_s2_3 * interview_data$ASSESSMENT_2_CAT + Z %*% ranef_3
  
  y_s3 <- rbinom(n, size = 1, prob = inv_logit(mu_s3))
  
  # Create data frame 
  interview_data <- 
    interview_data %>%
    dplyr::mutate(
      HIRE_REC = y_s3
    ) %>%
    dplyr::mutate(
      INTERVIEWER_GENDER = dplyr::case_when(
        INTERVIEWER_GENDER == 1 ~ "Male",
        TRUE ~ "Female"
      ),
      CANDIDATE_GENDER = dplyr::case_when(
        CANDIDATE_GENDER == 1 ~ "Male",
        TRUE ~ "Female"
      )
    )
  
  if(sparse_data) {
    interview_keep_id <- 
      interview_data %>%
      dplyr::mutate(
        ROW_ID = 1:nrow(interview_data)
      ) %>%
      dplyr::group_by(
        CANDIDATE_ID
      ) %>%
      dplyr::summarize(
        KEEP = list(sample(ROW_ID, 5))
      ) %>%
      dplyr::pull(
        KEEP
      ) %>%
      unlist(.)
    
    interview_data_sparse <-
      tibble::tibble(
        CANDIDATE_ID = rep(1:n_candidates, each = 5),
        ROW_ID = as.numeric(interview_keep_id)
      ) %>%
      dplyr::left_join(
        interview_data %>%
          dplyr::mutate(
            ROW_ID = 1:nrow(interview_data)
          ),
        by = c("CANDIDATE_ID", "ROW_ID")
      )
    
    output <- list(
      interview_data = interview_data,
      interview_data_sparse = interview_data_sparse
    )
  } else {
    output <- list(
      interview_data = interview_data,
      interview_data_sparse = NULL
    )
    
    return(output)
  }
  
  
}

# interaction_plot 
interaction_plot <- function(
  data,
  outcome
) {
  
  data <-
    data %>%
    dplyr::select(
      {{outcome}},
      INTERVIEWER_GENDER,
      CANDIDATE_GENDER
    ) %>%
    dplyr::group_by(
      INTERVIEWER_GENDER,
      CANDIDATE_GENDER
    ) %>%
    dplyr::summarize(
      MEAN_OUTCOME = mean({{outcome}})
    )
  
  ggplot2::ggplot(
    data,
    aes(x = INTERVIEWER_GENDER,
        y = MEAN_OUTCOME,
        fill = CANDIDATE_GENDER)
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = position_dodge()
    )
}





