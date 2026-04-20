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
  div(
    style = "max-width: 500px;",
    sliderInput(
      inputId = "selected_year",
      label = "Select year:",
      min = 2019,
      max = 2025,
      value = 2025,
      step = 1,
      sep = ""
    )
  ),
  plotOutput("leaders_plot", height = "750px")
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  leader_data <- reactive({
    whr_year <- whr |>
      filter(year == input$selected_year)

    winners <- bind_rows(
      whr_year |>
        slice_max(explained_by_log_gdp_per_capita, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Log GDP per capita"),

      whr_year |>
        slice_max(explained_by_social_support, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Social support"),

      whr_year |>
        slice_max(explained_by_healthy_life_expectancy, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Healthy life expectancy"),

      whr_year |>
        slice_max(explained_by_freedom_to_make_life_choices, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Freedom to make life choices"),

      whr_year |>
        slice_max(explained_by_generosity, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Generosity"),

      whr_year |>
        slice_max(explained_by_perceptions_of_corruption, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Perceptions of corruption"),

      whr_year |>
        slice_max(dystopia_residual, n = 1, with_ties = FALSE) |>
        mutate(leading_component = "Dystopia + residual")
    )

    leader_labels <- winners |>
      group_by(country) |>
      summarize(
        leaders = paste(unique(leading_component), collapse = ", "),
        .groups = "drop"
      )

    leader_countries <- winners |>
      distinct(country, .keep_all = TRUE) |>
      left_join(leader_labels, by = "country") |>
      mutate(
        label = paste0(country, " (", leaders, ")"),
        label = fct_reorder(label, life_eval)
      )

    leader_countries |>
      select(
        label,
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
    ggplot(leader_data(), aes(x = value, y = label, fill = component)) +
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
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10)
      )
  })
}

shinyApp(ui, server)