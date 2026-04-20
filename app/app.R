library(tidyverse)
library(shiny)
library(bslib)

# Load data -------------------------------------------------------------------

whr <- read_csv("data/whr.csv")

# UI --------------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(),
  titlePanel("Happiness component leaders over time"),
  p(
    "This app shows the countries that lead at least one explanatory component of happiness in the selected year."
  ),
  sliderInput(
    inputId = "selected_year",
    label = "Select year:",
    min = 2019,
    max = 2025,
    value = 2025,
    step = 1,
    sep = ""
  ),
  plotOutput("leaders_plot", height = "750px")
)


# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  leader_data <- reactive({
    whr_year <- whr |>
      filter(year == input$selected_year)

    leader_countries <- bind_rows(
      whr_year |> slice_max(explained_by_log_gdp_per_capita, n = 1, with_ties = FALSE),
      whr_year |> slice_max(explained_by_social_support, n = 1, with_ties = FALSE),
      whr_year |> slice_max(explained_by_healthy_life_expectancy, n = 1, with_ties = FALSE),
      whr_year |> slice_max(explained_by_freedom_to_make_life_choices, n = 1, with_ties = FALSE),
      whr_year |> slice_max(explained_by_generosity, n = 1, with_ties = FALSE),
      whr_year |> slice_max(explained_by_perceptions_of_corruption, n = 1, with_ties = FALSE),
      whr_year |> slice_max(dystopia_residual, n = 1, with_ties = FALSE)
    ) |>
      distinct(country, .keep_all = TRUE) |>
      mutate(
        country = fct_reorder(country, life_eval)
      )

    leader_countries |>
      select(
        country,
        life_eval,
        explained_by_log_gdp_per_capita,
        explained_by_social_support,
        explained_by_healthy_life_expectancy,
        explained_by_freedom_to_make_life_choices,
        explained_by_generosity,
        explained_by_perceptions_of_corruption,
        dystopia_residual
      ) |>
      pivot_longer(
        cols = explained_by_log_gdp_per_capita:dystopia_residual,
        names_to = "component",
        values_to = "value"
      ) |>
      mutate(
        component = recode(
          component,
          explained_by_log_gdp_per_capita = "Log GDP per capita",
          explained_by_social_support = "Social support",
          explained_by_healthy_life_expectancy = "Healthy life expectancy",
          explained_by_freedom_to_make_life_choices = "Freedom to make life choices",
          explained_by_generosity = "Generosity",
          explained_by_perceptions_of_corruption = "Perceptions of corruption",
          dystopia_residual = "Dystopia + residual"
        )
      )
  })

  output$leaders_plot <- renderPlot({
  ggplot(leader_data(), aes(x = value, y = country, fill = component)) +
    geom_col() +
    scale_x_continuous(limits = c(-0.3, NA)) +
    scale_fill_manual(
      values = c(
        "Log GDP per capita" = "#0072B2",
        "Social support" = "#E69F00",
        "Healthy life expectancy" = "#009E73",
        "Freedom to make life choices" = "#CC79A7",
        "Generosity" = "#56B4E9",
        "Perceptions of corruption" = "#D55E00",
        "Dystopia + residual" = "#F0E442"
      ),
      drop = FALSE
    ) +
    labs(
      title = paste("How happiness is composed for component leaders in", input$selected_year),
      subtitle = "Each country leads in at least one explanatory component of happiness",
      x = "Contribution to life evaluation score",
      y = NULL,
      fill = "Component"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  })
}

shinyApp(ui, server)