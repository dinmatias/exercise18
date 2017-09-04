
driftCollapse <- div(id = "driftParameters", class = "collapse",
                     fluidRow(
                       column(3, h4("Population 1 Parameters"))
                     ),
                     div(class = "well",
                           fluidRow(
                             column(3, "Effective Population Size (Ne)"),
                             column(3, "Initial allele A frequency")
                             ),
                           fluidRow(
                             column(3, textInput("PopSize1", NULL, value = 200, width = "50%")),
                             column(3,  sliderInput("InFreq1", NULL, min = 0, max = 1, 
                                                    value = 0.5, ticks = FALSE, width = "50%"))
                             )
                         ), # well
                     fluidRow(
                       column(3, h4("Evolution Parameter"))
                     ),
                     div(class = "well",
                         fluidRow(
                           column(3, "Number of Generations")
                           ),
                         fluidRow(
                           column(3, textInput("numGen", NULL, value = 200, width = "50%"))
                           )
                         ) # well
                     ) # collapse


bottleneckCollapse <- div(id = "botParameters", class = "collapse",
                          fluidRow(
                            column(3, h4("Bottleneck Parameters"))
                          ),
                          div(class = "well",
                                fluidRow(
                                  column(3, "Generation of Bottleneck"),
                                  column(3, "Proportion of Ne Post Bottleneck")
                                  ),
                                fluidRow(
                                  column(3, textInput("botPost", NULL, value = 200, width = "50%")),
                                  column(3, sliderInput("botPercent", NULL, min = 0, max = 1,
                                                        value = 0.5, ticks = FALSE, width = "50%"))
                                  )
                              ) # well
                          ) # collapse


geneflowCollapse <- div(id = "gfParameters", class = "collapse",
                        fluidRow(
                          column(3, h4("Population 2 Parameters"))
                        ),
                        div(class = "well",
                            fluidRow(
                              column(3, "Population 2 Ne"),
                              column(3, "Population 2 allele A Frequency")
                              ),
                            fluidRow(
                              column(3, textInput("popSize2", NULL, value = 200, width = "50%")),
                              column(3,  sliderInput("InFreq2", NULL, min = 0, max = 1,
                                                     value = 0.5, ticks = FALSE, width = "50%"))
                              )
                            ), # well
                        fluidRow(
                          column(3, h4("Gene Flow Parameters"))
                        ),
                        div(class = "well",
                            fluidRow(
                              column(3, "Average Number of Migrant 1 > 2"),
                              column(3, "Average Number of Migrant 2 > 1")
                              ),
                            fluidRow(
                              column(3, textInput("gfmig12", NULL, value = 1, width = "50%")),
                              column(3, textInput("gfmig21", NULL, value = 1, width = "50%"))
                              )
                            ) # well
                        ) # collapse




selectionCollapse <- div(id = "selParameters", class = "collapse",
                         fluidRow(
                           column(3, h4("Selection Parameters"))
                         ),
                         div(class = "well",
                             fluidRow(
                               column(3, "Fitness of AA individuals"),
                               column(3, "Fitness of Aa individuals"),
                               column(3, "Fitness of aa individuals")
                               ),
                             fluidRow(
                               column(3, textInput("fitAA", NULL, value = 0.95, width = "30%")),
                               column(3, textInput("fitAa", NULL, value = 0.90, width = "30%")),
                               column(3, textInput("fitaa", NULL, value = 0.95, width = "30%"))
                               )
                             ) # well
                         ) # collapse


plotOutput <- div(id="plotCollapse", class="collapse", 
                  fluidRow(column(8, 
                                  plotOutput("frequencyPlot", height=700,
                                             click = "plot_click",
                                             hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                             brush = brushOpts(id = "plot_brush"))),
                           column(4,
                                  br(),
                                  br(),
                                  br(),
                                  hr(),
                                  h4("Pointer Information"),
                                  div(class = "well",
                                      fluidRow(column(width = 3, "Generation: "), 
                                           column(1, textOutput("plot_hoverinfox")),
                                           column(width = 4, "Frequency allele A: "), 
                                           column(1 ,textOutput("plot_hoverinfoy")))
                                      ),
                                  hr(),
                                  h4("Generations Selected"),
                                  div(class = "well",
                                      textOutput("plot_brushedpoints")
                                      ),
                                  actionButton("censusList", "Add to Census")
                                  )
                           ))


censusOutput <- div(id="censusCollapse", class="collapse",
                  fluidRow(
                    column(6, height = "500px", DT::dataTableOutput("censusResults")
                    ))
                  )


# customCheckbox <- function(checkID, labelID, targetTag){
#   block1 <- paste("HTML(", "'", "<div class=", '"form-group shiny-input-container" style="width: 600px;">', sep = "")
#   block2 <- '<div class="squaredFour">'
#   block3 <- paste('<label data-toggle="collapse" data-target=', "#", targetTag, ' aria-expanded=', '"false"', ' aria-controls=', targetTag, '>', sep = "")
#   block4 <- paste('<input id=', checkID, ' type="checkbox" />', sep = "")
#   block5 <- paste('<span>', labelID, '</span>', sep = "")
#   block6 <- '</label>'
#   block7 <- '</div>'
#   block8 <- "</div>')"
#   
#   x <- eval(parse(text = paste(block1, block2, block3, block4, block5, block6, block7, block8, sep = "\n")))
#   
#   return(x)
# }

customCheckbox <- function(checkID, labelID, targetTag){
  block1 <- paste("HTML(", "'", "<div class=", '"form-group shiny-input-container" style="width: 600px;">', sep = "")
  block2 <- '<div class="checkbox">'
  block3 <- paste('<label data-toggle="collapse" data-target=', "#", targetTag, ' aria-expanded=', '"false"', ' aria-controls=', targetTag, '>', sep = "")
  block4 <- paste('<input id=', checkID, ' type="checkbox" />', sep = "")
  block5 <- paste('<span>', labelID, '</span>', sep = "")
  block6 <- '</label>'
  block7 <- '</div>'
  block8 <- "</div>')"

  x <- eval(parse(text = paste(block1, block2, block3, block4, block5, block6, block7, block8, sep = "\n")))

  return(x)
}


driftcheckbox <- customCheckbox(checkID = "driftcheckbox", targetTag = "driftParameters", labelID = "Genetic Drift")
gfcheckbox <- customCheckbox(checkID = "gfcheckbox", targetTag = "gfParameters", labelID = "Gene Flow")
botcheckbox <- customCheckbox(checkID = "botcheckbox", targetTag = "botParameters", labelID = "Bottleneck")
selcheckbox <- customCheckbox(checkID = "selcheckbox", targetTag = "selParameters", labelID = "Selection")

# data-toggle="collapse" data-target=', "#", targetTag, ' aria-expanded=', '"false"', ' aria-controls=', targetTag

