##-- Pacotes ----
require(dplyr)
require(reshape2)
source("dados/url_webservice.R")
##-- Setando a merda do diretório pq a porra do pacote precisa ----
setwd("~/Dropbox/outros_projetos/Cepesp/")
##-- Possíveis consultas ----
anos <- seq(1998, 2016, by = 2)
posicoes <- c("Vereador", "Prefeito", "Deputado Estadual", "Deputado Federal", "Senador", "Governador", "Presidente")
agregacoes <- c("Brasil", "Macro", "Estado", "Meso", "Micro", "Municipio", "Municipio-Zona", "Zona", "Votação Seção")
agregacoes_politicas <- c("Candidato", "Partido", "Coligação", "Consoliado")

tipo_eleicao <- data.frame(ano = anos, 
                           vereador = c(0, 1), prefeito = c(0, 1), 
                           dep_estadual = c(1, 0), dep_federal = c(1, 0), senador = c(1, 0), 
                           governador = c(1, 0), presidente = c(1, 0))
tipo_eleicao <- melt(data = tipo_eleicao, 1, variable.name = "posicoes", value.name = "consultar")

codigos <- data.frame(posicoes = c("vereador", "prefeito", 
                                   "dep_estadual", "dep_federal", "senador", 
                                   "governador", "presidente"),
                      posicoes_label = posicoes,
                      posicoes_codigo = c(13, 11, 7, 6, 5, 3, 1))

consultas <- merge(tipo_eleicao, codigos, by = "posicoes") %>%
  filter(consultar == 1) %>%
  arrange(posicoes_codigo)
 
for(i in 1:nrow(consultas)){
  cargo <- consultas$posicoes_codigo[i]
  cargo_label <- consultas$posicoes[i]
  ano <- consultas$ano[i]
  
  url <- url_webservice(cargo = cargo, ano = ano)
  arquivo <- paste0("~/Documents/cepesp/", paste0(cargo_label, "_", ano, ".csv"))
  
  if(file.exists(arquivo)){
    NULL
  } else{
    download.file(url = url, destfile = arquivo)  
  }
  
}