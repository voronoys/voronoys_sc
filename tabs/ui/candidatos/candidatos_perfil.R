perfil <- tabPanel(title = "Candidate profile", 
                   value = "candidatos_perfil",
                   br(), hr(),
                   column(width = 10,
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_ano", 
                                             label = "Year", 
                                             choices = anos, 
                                             selected = 2014, 
                                             options = list(`live-search` = TRUE))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_cargo", 
                                             label = "Position", 
                                             choices = cargos, 
                                             selected = "PRESIDENT",
                                             options = list(`live-search` = TRUE))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_turno", 
                                             label = "Round", 
                                             choices = c("1º round", "2º round"), 
                                             selected = "1º",
                                             options = list(`live-search` = TRUE))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_partido", 
                                             label = "Party", 
                                             choices = levels(factor(x = c("All parties", partidos),
                                                                     levels = c("All parties", partidos))), 
                                             selected = "All parties",
                                             options = list(`live-search` = TRUE,
                                                            `none-selected-text` = "None selected"))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_estado", 
                                             label = "State", 
                                             choices = levels(factor(x = c("All states", estados),
                                                                     levels = c("All states", estados))), 
                                             selected = "Todos os estados",
                                             options = list(`live-search` = TRUE,
                                                            `none-selected-text` = "None selected"))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_cpf", 
                                             label = "Candidate", 
                                             choices = NULL, 
                                             selected = NULL,
                                             options = list(`live-search` = TRUE,
                                                            `none-selected-text` = "None selected"))
                          )
                   ), 
                   column(width = 2, style = "padding-top: 50px;",
                          actionBttn(inputId = "perfil_candidato_gerar_visualizacoes", 
                                     label = "Select", 
                                     style = "fill", 
                                     color = "success", 
                                     icon = icon("check"), 
                                     size = "sm") 
                   ),
                   conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes < 1",
                                    column(width = 12,
                                           br(), 
                                           wellPanel(
                                             HTML("<h1>Candidates profile<h1>"),
                                             HTML("<h4>How old is your candidate? Is he/she graduated? Where is he/she from? How many votes in your city? In this tab you can answer this and other questions. 
                                                  <h4>")
                                           )
                                    )
                   ),
                   ##-- Outputs ----
                   column(width = 12,
                          column(width = 6,
                                 conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes > 0",
                                                  HTML("<center><h1>PROPORTION OF VOTES</h1></center>"),
                                                  br(), 
                                                  column(width = 12,
                                                         withSpinner(leafletOutput("perfil_candidatos_mapa", height = "500px"), type = 6)
                                                  )
                                 )
                          ),
                          column(width = 6,
                                 conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes > 0",
                                                  HTML("<center><h1>CANDIDATE PROFILE</h1></center>"),
                                                  column(width = 12,
                                                         withSpinner(uiOutput("perfil_candidato"), type = 6)
                                                  )           
                                 )
                          )
                   )
)