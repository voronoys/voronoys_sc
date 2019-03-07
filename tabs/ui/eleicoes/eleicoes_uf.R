eleicoes_uf <- tabPanel(title = "State level", 
                        value = "uf",
                        br(), hr(),
                        ##-- Botões ----
                        column(width = 10,
                               ##-- + Ano ----
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_ano_uf", 
                                                  label = "Year", 
                                                  choices = 2014, 
                                                  selected = 2014, 
                                                  options = list(`live-search` = TRUE))
                               ),
                               ##-- + Cargo ----
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_cargo_uf", 
                                                  label = "Position", 
                                                  choices = cargos, 
                                                  selected = "PRESIDENT",
                                                  options = list(`live-search` = TRUE))
                               ),
                               ##-- + Turno ----
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_turno_uf", 
                                                  label = "Round", 
                                                  choices = c("1º round", "2º round"), 
                                                  selected = "1º round",
                                                  options = list(`live-search` = TRUE))
                               ),
                               ##-- + Estado ----
                               column(width = 2,
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
                        column(width = 2, style = "padding-top: 50px;",
                               actionBttn(inputId = "eleicoes_gerar_visualizacoes_uf", 
                                          label = "Select", 
                                          style = "fill", 
                                          color = "success", 
                                          icon = icon("check"), size = "sm") 
                        ),
                        conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf < 1",
                                         column(width = 12,
                                                br(), 
                                                wellPanel(
                                                  HTML("<h1>Elections: State level<h1>"),
                                                  HTML("<h4>In this tab the user can investigate the winners in each state/city for the Senator and Governor position. 
                                                       It is interesting to see hot areas of each party. As soon as possible, other positions will be added.<h4>")
                                                )
                                         )
                        ),
                        ##-- Outputs ----
                        column(width = 12,
                               ##-- + Mapa do Brasil ----
                               column(width = 4,
                                      conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                       HTML("<center><h1>TOP VOTED BY STATE</h1></center>"),
                                                       br(),
                                                       withSpinner(leafletOutput("mapa_uf_geral_uf"), type = 6)
                                      )
                               ),
                               ##-- + Mapa dos municípios ----
                               column(width = 4,
                                      conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                       HTML("<center><h1>ELECTION BY CITY</h1></center>"),
                                                       br(),
                                                       withSpinner(leafletOutput("mapa_mun_geral_uf"), type = 6)
                                                       
                                      )
                               ),
                               ##-- + Gráfico de barras ----
                               column(width = 4,
                                      conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                       HTML("<center><h1>VOTES BARPLOT (%)</h1></center>"),
                                                       br(),
                                                       withSpinner(plotlyOutput("barras_geral_uf"), type = 6)
                                                       
                                      )
                               )
                        )
)