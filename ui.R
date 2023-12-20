library(shiny);
library(shinyjs);
library(bslib);

# Define UI for application that draws a histogram
fillPage(
    title='BejeweRed',theme=bs_theme(bootswatch='quartz'), padding=c('0','1vw'),
    useShinyjs(),
    # head
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bejewered.css")
    ),
    DT::dataTableOutput('bjwdt'),
    textOutput('score'),
    if(file.exists('debug')) actionButton('debug','debug') else c(),
    textOutput('debugoutput')
)
