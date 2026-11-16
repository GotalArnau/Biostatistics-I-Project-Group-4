
# Set up ------------------------------------------------------------------
library(tidyverse)
library(ggpubr)

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



# Description and plots -------------------------------------------------------------------

#aggregate no. cases for all years by age group and plot
merged_data |> ggplot(
  aes(agegroup, n_cases, fill = sex)
) + geom_col(position = "dodge") + labs(fill = "Sex", x = "Age Group", y = "# Cases")

#no. of cases each year by sex
merged_data |>
  group_by(year, sex) |>
  summarise(total_cases = sum(n_cases))

merged_data |> ggplot(
  aes(year, n_cases, fill = sex)
) + geom_col() + labs(fill = "Sex", x = "Year", y = "# Cases")

#population size for each year presented in each age group for each sex.

merged_data |> 
  ggplot(aes(x = year, y = agegroup, fill = n_pop)) +
  geom_tile() + 
  facet_wrap(~ sex) + 
  scale_fill_viridis_c(labels = scales::comma, option = "magma") +
  labs(fill = "Population Size", x = "Year", y = "Age Group") +
  theme_minimal()

# Analysis/modeling ----------------------------------------------------------------



# Age standardization -----------------------------------------------------



