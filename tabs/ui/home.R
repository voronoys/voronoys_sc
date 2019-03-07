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
                          HTML("<h1><b>VORONOYS</b></h1>"),
                          HTML("<h4><b>VORONOYS</b> is a project which explores Brazilian elections by the usage of data science. 
                               Our aim is to show, clearly and directly, relevant information about election results using some underexplored point of views.
                               To achieve this, we tackle subjects as data visualization and voters socio-demographic measures by each candidate party. 
                               Using the Voronoi tessellation methodology it was possible to report the election results in a more disaggregated level. 
                               The user can compare important socio-demographic variables with the proportion of votes in each party and then understand each party 
                               voter profile pattern . Besides that, several candidates and results information can be found and explored in this app.
                               .</h4>")
                        )
                 )
)
