
# Set up ------------------------------------------------------------------
library(tidyverse)
library(ggpubr)

# Import and Clean Data --------------------------------------------------- 
cases_data <- read_tsv(
  "cases-2.tsv",
  col_types = cols(
    sex = col_factor(),
    year = col_double(),
    agegroup = col_factor(ordered = TRUE),
    n = col_double()
  )
)

population_data <- read_tsv(
  "population-2.tsv",
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
# Q12 : Age-standardized incidence rates
data_2022 <- population_data |> 
  filter(year == 2022) |>
  select(sex, agegroup, n_pop_2022 = n_pop)

data_std <- merged_data |>
  mutate(incidence_rate = n_cases / n_pop) |>
  left_join(data_2022, by = c("sex", "agegroup")) |>
  group_by(year, sex) |>
  summarise(rate = sum(incidence_rate * n_pop_2022) / sum(n_pop_2022) * 100000, .groups = "drop_last" ) 

plot_std <- ggplot(data_std, aes(x = year, y = rate)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, colour = "blue") +
  facet_wrap(~sex) +
  labs(title = "Age-standardized incidence rate of colon cancer over calendar year",
       x = "Calendar year",
       y = "(Age-standardized) Incidence rate per 100,000"
  ) +  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 15))

plot_std

# Comparison q6 vs q12
data_std <- mutate(data_std, analysis = "Age-standardized rate")
crude_data <- merged_data |>
  group_by(year, sex) |>
  summarise(rate = sum(n_cases) / sum(n_pop) * 100000, .groups = "drop") |>
  mutate(analysis = "Crude rate")
comp_data <- bind_rows(crude_data, data_std)

comp_plot <- ggplot(comp_data, aes(x = year, y = rate, color = analysis)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  facet_wrap(~sex) +
  labs(
    title = "Comparison of age-standardized (Q12) vs crude (Q6) incidence rates",
    x = "Calendar year",
    y = "Incidence rate per 100,000",
    color = "Incidence rate analysis type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 15)
  )
comp_plot
