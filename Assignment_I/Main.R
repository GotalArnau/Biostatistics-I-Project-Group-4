
# Set up ------------------------------------------------------------------
library(tidyverse)

# Import and Clean Data ---------------------------------------------------

cases_data <- read_tsv(
  "Assignment_I/cases-2.tsv",
  col_types = cols(
    sex = col_factor(),
    year = col_double(),
    agegroup = col_factor(ordered = TRUE),
    n = col_double()
  )
)

population_data <- read_tsv(
  "Assignment_I/population-2.tsv",
  col_types = cols(
    sex = col_factor(),
    year = col_double(),
    agegroup = col_factor(ordered = TRUE),
    n_pop = col_double()
  )
)

#Joins the two df by matching values on vectors
merged_data <- full_join(cases_data, population_data)


# Plots -------------------------------------------------------------------



