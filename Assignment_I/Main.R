
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

#Joins the two df by matching values on vectors and rename columns
merged_data <- full_join(cases_data, population_data) |> rename("n_cases" = "n")


# Plots -------------------------------------------------------------------

#aggregate no. cases for all years by age group and plot
merged_data |> ggplot(
  aes(agegroup, n_cases, fill = sex)
) + geom_col() 

#
merged_data |> ggplot(
  aes(year, n_cases, fill = sex)
) + geom_col() 


# Analysis/modeling ----------------------------------------------------------------



# Age standardization -----------------------------------------------------



