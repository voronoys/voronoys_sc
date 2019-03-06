partido_geral <- tabPanel(title = "General analysis", 
                          value = "partidos_geral",
                          br(), hr(),
                          column(width = 10,
                                 column(width = 2,
                                        pickerInput(inputId = "partido_geral_ano", 
                                                    label = "Year", 
                                                    choices = anos, 
                                                    selected = 2014, 
                                                    options = list(`live-search` = TRUE))
                                 ),
                                 column(width = 2,
                                        pickerInput(inputId = "partido_geral_cargo", 
                                                    label = "Position", 
                                                    choices = cargos, 
                                                    selected = "GOVERNOR", 
                                                    options = list(`live-search` = TRUE))
                                 )
                          ),
                          column(width = 2, style = "padding-top: 50px;",
                                 actionBttn(inputId = "partidos_gerar_visualizacoes1", 
                                            label = "Select", 
                                            style = "fill", 
                                            color = "success", 
                                            icon = icon("check"), size = "sm") 
                          ),
                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes1 < 1",
                                           column(width = 12,
                                                  br(), 
                                                  wellPanel(
                                                    HTML("<h1>General analisys<h1>"),
                                                    HTML("<h4>
                                                         Nowadays in Brazil we have <b>35 parties</b>. Here it is possible to see the coalittions for each position. 
                                                         <h4>")
                                                  )
                                           )
                          ),
                          column(width = 12,
                                 column(width = 6,
                                        conditionalPanel(condition = "input.partidos_gerar_visualizacoes1 > 0",
                                                         br(), hr(), br(),
                                                         HTML("<center><h1>COALITIONS HEATMAP</h1></center>"),
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