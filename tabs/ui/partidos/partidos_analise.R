partido_analise <- tabPanel(title = "Party analysis", 
                            value = "partidos",
                            br(), hr(),
                            column(width = 4,
                                   column(width = 6,
                                          pickerInput(inputId = "partido_ano", 
                                                      label = "Year", 
                                                      choices = anos, 
                                                      selected = 2014, 
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   column(width = 6,
                                          pickerInput(inputId = "partido_cargo", 
                                                      label = "Position", 
                                                      choices = cargos, 
                                                      selected = 1,
                                                      options = list(`live-search` = TRUE))
                                   )
                            ), 
                            column(width = 8,
                                   br(), 
                                   actionBttn(inputId = "partidos_gerar_visualizacoes", 
                                              label = "Select", 
                                              style = "fill", 
                                              color = "success", 
                                              icon = icon("check")) 
                            ),
                            ##-- Outputs ----
                            column(width = 12,
                                   conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                    hr(),
                                                    HTML("<center>"),
                                                    pickerInput(inputId = "partido_partido_donuts", 
                                                                label = "Party", 
                                                                choices = levels(factor(x = c("All parties", partidos),
                                                                                        levels = c("All parties", partidos))), 
                                                                selected = "All parties",
                                                                options = list(`live-search` = TRUE,
                                                                               `none-selected-text` = "None selected")),
                                                    HTML("</center>")
                                   ),
                                   column(width = 4,
                                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                           HTML("<center><h1>GENDER PROPORTION</h1></center>"),
                                                           column(width = 12,
                                                                  withSpinner(plotlyOutput("donut_sexo"), type = 6)
                                                           )           
                                          )
                                   ),
                                   column(width = 4,
                                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                           HTML("<center><h1>ETHNICITY PROPORTION</h1></center>"),
                                                           column(width = 12,
                                                                  withSpinner(plotlyOutput("donut_raca"), type = 6)
                                                           )           
                                          )
                                   ),
                                   column(width = 4,
                                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                           HTML("<center><h1>SCHOLARITY</h1></center>"),
                                                           column(width = 12,
                                                                  withSpinner(dataTableOutput("tabela"), type = 6)
                                                           )           
                                          )
                                   )
                            )
)