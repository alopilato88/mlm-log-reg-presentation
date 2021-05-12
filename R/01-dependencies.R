######################################################################
# This script loads all of the packages and user-defined functions   #
# used by all remaining scripts in this repo.                        #
######################################################################

# Set seed for reproducibility
set.seed(1414214)

# Load packages
library(dplyr)
library(tidyr)
library(tibble)
library(magrittr)
library(lme4)
library(stringr)
library(mvtnorm)
library(purrr)

# Source user-defined functions
source("../R/02-functions.R")
