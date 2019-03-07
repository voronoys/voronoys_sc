perfil_eleitorado <- tabPanel(title = "Voter profile", 
                              value = "candidatos_perfil_eleitorado",
                              br(), hr(),
                              column(width = 10,
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_ano", 
                                                        label = "Year", 
                                                        choices = 2014,
                                                        selected = 2014)
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_cargo", 
                                                        label = "Position", 
                                                        choices = "PRESIDENT",
                                                        selected = "PRESIDENT")
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_turno", 
                                                        label = "Round", 
                                                        choices = c("1º round", "2º round"), 
                                                        selected = "1º",
                                                        options = list(`live-search` = TRUE))
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_partido", 
                                                        label = "Party", 
                                                        choices = levels(factor(x = partidos,
                                                                                levels = partidos)), 
                                                        selected = "PT",
                                                        options = list(`live-search` = TRUE,
                                                                       `none-selected-text` = "None selected"))
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_estado", 
                                                        label = "State", 
                                                        choices = "SP", 
                                                        selected = "SP",
                                                        options = list(`live-search` = TRUE,
                                                                       `none-selected-text` = "None selected"))
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_municipio", 
                                                        label = "City", 
                                                        choices = municipios, 
                                                        selected = "3550308",
                                                        options = list(`live-search` = TRUE,
                                                                       `none-selected-text` = "None selected"))
                                     )
                              ),
                              column(width = 2, style = "padding-top: 50px;",
                                     actionBttn(inputId = "perfil_candidato_voronoi_gerar_visualizacoes", 
                                                label = "Select", 
                                                style = "fill", 
                                                color = "success", 
                                                icon = icon("check"), size = "sm"),
                                     HTML("&nbsp"),
                                     actionBttn(inputId = "perfil_candidato_voronoi_metodologia", 
                                                label = "Methodology", 
                                                style = "fill", 
                                                color = "danger", 
                                                icon = icon("book"), size = "sm")
                              ),
                              conditionalPanel(condition = "input.perfil_candidato_voronoi_gerar_visualizacoes < 1",
                                               column(width = 12,
                                                      br(), 
                                                      wellPanel(
                                                        HTML("<h1>Voronoi Data Linkage: Extracting data from polygons to points<h1>"),
                                                        HTML("<h4>
                                                              In Brazil, electoral data has four main aggregation levels: state, municipality, electoral zone and electoral 
                                                              section. The lower one, the electoral section, does not have any administrative division, i.e., it is just an 
                                                              address and not an area. Considering that social-demographic variables can be useful to explain the election 
                                                              outcome, we are interested in aggregating such kind of data, provided by the Instituto Brasileiro de Geografia e 
                                                              Estatistica (IBGE) in small areas called census sectors. The variables extracted from the census sectors to the 
                                                              electoral sections are income, household, illiteracy rate, proportion of white people, and proportion of women. 
                                                              The outcome of interest, available on electoral section data, is the proportion of votes in each party. As a 
                                                              consequence,  it is possible to understand voters  characteristics each party as well as a smoothed result of the 
                                                              election at an electoral section level (voronoi cells).
                                                             <h4>")
                                                      )
                                               )
                              ),
                              column(width = 12,
                                     conditionalPanel(condition = "input.perfil_candidato_voronoi_gerar_visualizacoes > 0",
                                                      column(width = 6,
                                                             HTML("<center><h1>VOTES BY ELECTORAL SECTIONS</h1></center>")
                                                      ),
                                                      column(width = 6,
                                                             HTML("<center><h1>SOCIO-DEMOGRAPHIC VARIABLES</h1></center>"),
                                                             HTML("<center>"),
                                                             pickerInput(inputId = "perfil_candidato_voronoi_variavel", 
                                                                         label = "Variável", 
                                                                         choices = c("Income", "Income (categories)",
                                                                                     "% white", "% not white",
                                                                                     "% illiteracy",
                                                                                     "% women", "% men"),
                                                                         selected = "Income"),
                                                             HTML("</center>")
                                                      ),
                                                      column(width = 12,
                                                             withSpinner(uiOutput("mapa_voronoi", height = "500px"), type = 6)
                                                      )
                                     )
                              )
)