eleicoes_brasil <- tabPanel(title = "Federal level", 
                            value = "brasil",
                            br(), hr(),
                            ##-- Botões ----
                            column(width = 8,
                                   ##-- + Ano ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_ano_br", 
                                                      label = "Year", 
                                                      choices = sort(anos, decreasing = T), 
                                                      selected = 2014, 
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Cargo ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_cargo_br", 
                                                      label = "Position", 
                                                      choices = list("PRESIDENT" = 1), 
                                                      selected = 1,
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Turno ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_turno_br", 
                                                      label = "Round", 
                                                      choices = c("1º round", "2º round"), 
                                                      selected = "1º round",
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Estado ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_estado_br", 
                                                      label = "State", 
                                                      choices = levels(factor(x = c("All states", estados),
                                                                              levels = c("All states", estados))), 
                                                      selected = "All states",
                                                      options = list(`live-search` = TRUE,
                                                                     `none-selected-text` = "None selected"))
                                   )
                            ), 
                            ##-- Visualizar ----
                            column(width = 4,
                                   column(width = 12,
                                          br(),
                                          actionBttn(inputId = "eleicoes_gerar_visualizacoes_br", 
                                                     label = "Select", 
                                                     style = "fill", 
                                                     color = "success", 
                                                     icon = icon("check")) 
                                   )
                            ),
                            ##-- Outputs ----
                            column(width = 12,
                                   ##-- + Mapa do Brasil ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                                           HTML("<center><h1>ELECTIONS BY STATE</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_uf_geral_br"), type = 6)
                                          )
                                   ),
                                   ##-- + Mapa dos municípios ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                                           HTML("<center><h1>ELECTIONS BY CITY</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_mun_geral_br"), type = 6)
                                                           
                                          )
                                   ),
                                   ##-- + Gráfico de barras ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                                           HTML("<center><h1>VOTES % BARPLOT</h1></center>"),
                                                           br(),
                                                           withSpinner(plotlyOutput("barras_geral_br"), type = 6)
                                                           
                                          )
                                   )
                            )
)