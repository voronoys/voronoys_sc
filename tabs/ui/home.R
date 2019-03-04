home <- tabPanel(title = "Home", 
                 value = "home",
                 hr(),
                 br(), br(),
                 HTML("<h1><center>WELCOME TO <b>VORONOYS</b> WEBPAGE...</center></h1>"),
                 br(), br(), br(), br(),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "General analysis", cor = cores[1], icon = "brasil.png", id = "analise_geral")
                        ),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "Party analysis", cor = cores[2], icon = "flag.png", id = "analise_partidos")
                 ),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "Candidates", cor = cores[3], icon = "person.png", id = "analise_candidatos")
                 ),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "About us", cor = cores[4], icon = "sobre.png", id = "sobre")
                 ),
                 column(width = 12,
                        br(), br(), br(), br(),
                        wellPanel(
                          #HTML("<h2>VORONOYS é um projeto que traz o universo da ciência de dados aplicado às eleições brasileiras. Nossa visão é transmitir, de forma clara e objetiva, informações relevantes sobre resultados eleitorais. Nossas soluções contemplam assuntos como visualização de dados e elucidação do perfil socioeconômica dos votantes de cada partido.</h2>")
                          HTML("<h2><b>VORONOYS</b> is a project that brings the data science thoughts applied to Brazilian elections.
                                Our aim is to show, clearly and directly, relevant information about election results using some underexplored point of views.
                                To achieve this we tackle subjects as data vizualization and voters socioecoonomic  measures of each party.</h2>")
                        )
                 )
)
