
library(shiny)


source("functions/uifunctions.R")

shinyUI(
  fluidPage(
    
    # theme = "checkbox.css",
  
    tags$head(tags$style(HTML(' .irs-min, .irs-max {
                              visibility: hidden !important;
                              }'))),
    shinyjs::useShinyjs(),

    h4("Exercise 18: Microevolutionary Processes and the Hardy-Weindberg Principle"),
    
    hr(),
    
    h4("Input Section"),
    
    hr(),
    
    driftcheckbox, 
    
    
    driftCollapse,
    
    hr(),
    
    botcheckbox,
    
    bottleneckCollapse,
    
    hr(),
    
    gfcheckbox, 
    
    
    geneflowCollapse,
    
    hr(),
    
    
    selcheckbox,
    
    
    selectionCollapse,
    
    hr(),
    
    fluidRow(column(2, offset = 5, actionButton("sim", "Simulate"))),
    
    hr(),
    
    # Output Page
    h4("Output Section"),
    
    hr(),
    
    HTML('<button id="showPlot" type="button" data-toggle="collapse" 
         data-target=#plotCollapse aria-expanded="false" 
         aria-controls=plotCollapse class="btn btn-default action-button" 
         data-text-swap="Hide Plot">Show Plot</button>'),
    
    hr(),
    
    plotOutput,
    
    hr(),
    
    HTML('<button id="showCensus" type="button" data-toggle="collapse" 
         data-target=#censusCollapse aria-expanded="false" 
         aria-controls=censusCollapse class="btn btn-default action-button" 
         data-text-swap="Hide Plot">Show Census Result</button>'),
    
    censusOutput,
    
    hr()
    
    )
  
  
    )
