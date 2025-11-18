# libraries
library(tidyverse)

# Function for grouping data based on survival status
subset_deceased <- function(data) {
  data |>
    filter(survival_status == "Dead")
}

# Function for grouping data based on survival status
group_surv <- function(data) {
  data |>
    group_by(survival_status)
}