tab_files <- list.files(path = "tabs/ui/candidatos", full.names = T)
suppressMessages(lapply(tab_files, source))

candidato <- tabPanel(title = "Candidates", 
                      value = "candidatos",
                      hr(),
                      tabsetPanel(
                        perfil_eleitorado,
                        perfil
                      )
)