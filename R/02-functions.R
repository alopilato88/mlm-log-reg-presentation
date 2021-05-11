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
