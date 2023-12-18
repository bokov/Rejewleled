library(shiny);
library(shinyjs);

# Define UI for application that draws a histogram
fluidPage(
    useShinyjs(),
    # head
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bejewered.css")
    ),
    # Application title
    titlePanel("Bejewered"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          actionButton('debug','debug'),
          textOutput('score'),
          textOutput('debugoutput')
          ),

        # Show a plot of the generated distribution
        mainPanel(DT::dataTableOutput('bjwdt'))
    )
)
