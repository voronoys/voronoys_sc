eleicoes_uf <- tabPanel(title = "State level", 
                            value = "uf",
                            br(), hr(),
                            ##-- Botões ----
                            column(width = 8,
                                   ##-- + Ano ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_ano_uf", 
                                                      label = "Year", 
                                                      choices = sort(anos, decreasing = T), 
                                                      selected = 2014, 
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Cargo ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_cargo_uf", 
                                                      label = "Position", 
                                                      choices = cargos, 
                                                      selected = "PRESIDENT",
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Turno ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_turno_uf", 
                                                      label = "Round", 
                                                      choices = c("1º round", "2º round"), 
                                                      selected = "1º round",
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Estado ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_estado_uf", 
                                                      label = "State", 
                                                      choices = levels(factor(x = estados,
                                                                              levels = estados)), 
                                                      selected = "AC",
                                                      options = list(`live-search` = TRUE,
                                                                     `none-selected-text` = "None selected"))
                                   )
                            ), 
                            ##-- Visualizar ----
                            column(width = 4,
                                   column(width = 12,
                                          br(),
                                          actionBttn(inputId = "eleicoes_gerar_visualizacoes_uf", 
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
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                           HTML("<center><h1>TOP BY STATE</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_uf_geral_uf"), type = 6)
                                          )
                                   ),
                                   ##-- + Mapa dos municípios ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                           HTML("<center><h1>ELECTION BY CITIES</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_mun_geral_uf"), type = 6)
                                                           
                                          )
                                   ),
                                   ##-- + Gráfico de barras ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                           HTML("<center><h1>VOTES % BARPLOT</h1></center>"),
                                                           br(),
                                                           withSpinner(plotlyOutput("barras_geral_uf"), type = 6)
                                                           
                                          )
                                   )
                            )
)