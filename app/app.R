# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
# add other packages as needed

# Load data --------------------------------------------------------------------

# Define UI --------------------------------------------------------------------

ui <- ...

# Define server function -------------------------------------------------------

server <- function(input, output, session) {
  ...
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
