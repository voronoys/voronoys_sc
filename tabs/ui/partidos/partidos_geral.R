partido_geral <- tabPanel(title = "General analysis", 
                          value = "partidos_geral",
                          br(), hr(),
                          column(width = 4,
                                 column(width = 6,
                                        pickerInput(inputId = "partido_geral_ano", 
                                                    label = "Year", 
                                                    choices = anos, 
                                                    selected = 2014, 
                                                    options = list(`live-search` = TRUE))
                                 ),
                                 column(width = 6,
                                        pickerInput(inputId = "partido_geral_cargo", 
                                                    label = "Position", 
                                                    choices = cargos, 
                                                    selected = "GOVERNOR", 
                                                    options = list(`live-search` = TRUE))
                                 )
                          ),
                          column(width = 8,
                                 column(width = 4,
                                        br(),
                                        actionBttn(inputId = "partidos_gerar_visualizacoes1", 
                                                   label = "Select", 
                                                   style = "fill", 
                                                   color = "success", 
                                                   icon = icon("check")) 
                                 )
                          ),
                          column(width = 12,
                                 column(width = 6,
                                        conditionalPanel(condition = "input.partidos_gerar_visualizacoes1 > 0",
                                                         br(), hr(), br(),
                                                         HTML("<center><h1>HEATMAP COALITIONS</h1></center>"),
                                                         column(width = 12,
                                                                withSpinner(plotlyOutput("heatmap_coligacoes"), 6)
                                                         )           
                                        )
                                 ),
                                 column(width = 6,
                                        conditionalPanel(condition = "input.partidos_gerar_visualizacoes1 > 0 & input.partido_geral_cargo == 11",
                                                         br(), hr(), br(),
                                                         HTML("<center><h1>WINNERS PARTIES</h1></center>"),
                                                         column(width = 12,
                                                                withSpinner(leafletOutput("mapa_partidos_cid"), type = 6)
                                                         )           
                                        )
                                 )
                          )
)