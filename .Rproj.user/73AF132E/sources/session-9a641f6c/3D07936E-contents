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

#function for generating counts
N_into_table <- function(input_data, variab, output_tab) {
  variab_tab <- input_data |>
    group_by(Disease, input_data[,variab]) |>
    summarise(N_variab = n()) |>
    pivot_wider(names_from = variab,
                values_from = N_variab)
  output_tab <- full_join(output_tab, variab_tab, join_by(Disease))
}

#function for generating percentages from counts
Perc <- function(input_data, column, new_colname) {
  new_colname_sym <- sym(new_colname)
  input_data |>
    mutate( {{ new_colname_sym }} := str_c( {{ column }}, " (", round( {{ column }} / N * 100, digits = 1), " %)")) |>
    select(! {{ column }} )
}