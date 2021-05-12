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









