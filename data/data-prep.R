# Data cleaning and saving as CSV code goes here

library(tidyverse)
library(readxl)

whr_raw <- read_excel("data/WHR26_Data_Figure_2.1.xlsx")

whr <- whr_raw |>
  rename(
    year = Year,
    rank = Rank,
    country = `Country name`,
    life_eval = `Life evaluation (3-year average)`,
    lower_whisker = `Lower whisker`,
    upper_whisker = `Upper whisker`,
    explained_by_log_gdp_per_capita = `Explained by: Log GDP per capita`,
    explained_by_social_support = `Explained by: Social support`,
    explained_by_healthy_life_expectancy = `Explained by: Healthy life expectancy`,
    explained_by_freedom_to_make_life_choices = `Explained by: Freedom to make life choices`,
    explained_by_generosity = `Explained by: Generosity`,
    explained_by_perceptions_of_corruption = `Explained by: Perceptions of corruption`,
    dystopia_residual = `Dystopia + residual`
  )

write_csv(whr, "data/whr.csv")


