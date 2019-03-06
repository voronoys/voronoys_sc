shinyServer(function(input, output, session){
  ##-- HOME ----
  source("tabs/server/home.R", local = TRUE)
  ##-- ELEIÇÕES ----
  source("tabs/server/eleicoes/eleicoes_brasil.R", local = TRUE)
  source("tabs/server/eleicoes/eleicoes_uf.R", local = TRUE)
  ##-- PARTIDOS ----
  source("tabs/server/partidos/partidos_geral.R", local = TRUE)
  source("tabs/server/partidos/partidos_analise.R", local = TRUE)
  ##-- CANDIDATOS ----
  source("tabs/server/candidatos/candidatos_perfil.R", local = TRUE)
  source("tabs/server/candidatos/candidatos_voronoi.R", local = TRUE)
  ##-- SOBRE ----
  source("tabs/server/sobre.R", local = TRUE)
})