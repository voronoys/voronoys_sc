##-- Pacotes ----
require(dplyr)
require(data.table)
require(reshape2)
pasta_arqs <- "~/Documents/cepesp/"
##-- Presidente ----
posicao <- "presidente"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

presidente <- bind_rows(lista)

saveRDS(object = presidente, file = "data/presidente.RDS")
##-- Senador ----
posicao <- "senador"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

senador <- bind_rows(lista)

saveRDS(object = senador, file = "data/senador.RDS")
##-- Governador ----
posicao <- "governador"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

governador <- bind_rows(lista)

saveRDS(object = governador, file = "data/governador.RDS")
##-- Deputado federal ----
posicao <- "dep_federal"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

dep_federal <- bind_rows(lista)

saveRDS(object = dep_federal, file = "data/dep_federal.RDS")
##-- Deputado estadual ----
posicao <- "dep_estadual"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

dep_estadual <- bind_rows(lista)

saveRDS(object = dep_estadual, file = "data/dep_estadual.RDS")
##-- Prefeito ----
posicao <- "prefeito"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

prefeito <- bind_rows(lista)

saveRDS(object = prefeito, file = "data/prefeito.RDS")
##-- Vereador ----
posicao <- "vereador"

arqs_posicao <- list.files(path = pasta_arqs)[grep(list.files(path = pasta_arqs), pattern = posicao)]
arqs_posicao <- paste0(pasta_arqs, arqs_posicao)

lista <- lapply(arqs_posicao, fread)
n_linhas <- sapply(lista, nrow) != 0
lista <- lista[n_linhas] 

vereador <- bind_rows(lista)

saveRDS(object = vereador, file = "data/vereador.RDS")
##-- Chaves ----
# arqs <- list.files(path = "dados/data", full.names = T)[-1]
# names(arqs) <- substr(x = basename(arqs), start = 1, nchar(basename(arqs))-4)
# dados <- lapply(arqs, readRDS)

vars_geral <- c("ANO_ELEICAO", "CODIGO_CARGO", "DESCRICAO_CARGO", 
                "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_PARTIDO", 
                "CPF_CANDIDATO", "NOME_CANDIDATO")
vars_estadual <- c("ANO_ELEICAO", "CODIGO_CARGO", "DESCRICAO_CARGO", 
                   "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_PARTIDO", 
                   "UF", "CPF_CANDIDATO", "NOME_CANDIDATO")
vars_municipal <- c("ANO_ELEICAO", "CODIGO_CARGO", "DESCRICAO_CARGO", 
                    "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_PARTIDO", 
                    "UF", "COD_MUN_IBGE", "NOME_MUNICIPIO", 
                    "CPF_CANDIDATO", "NOME_CANDIDATO")

presidente_key <- unique(presidente[, vars_geral, with = F])
senador_key <- unique(senador[, vars_estadual, with = F])
governador_key <- unique(governador[, vars_estadual, with = F])
dep_federal_key <- unique(dep_federal[, vars_estadual, with = F])
dep_estadual_key <- unique(dep_estadual[, vars_estadual, with = F])
prefeito_key <- unique(prefeito[, vars_municipal, with = F])
vereador_key <- unique(vereador[, vars_municipal, with = F])

chaves <- bind_rows(presidente_key,
                    senador_key,
                    governador_key,
                    dep_federal_key,
                    dep_estadual_key,
                    prefeito_key,
                    vereador_key)

saveRDS(object = chaves, file = "data/geral/chaves.RDS") 
