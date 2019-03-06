##-- Pacotes ----
#devtools::install_github("Cepesp-Fgv/cepesp-r")
library(dplyr)
library(dbplyr)
library(data.table)
library(reshape2)
library(tidyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#devtools::install_github('andrewsali/shinycssloaders')
library(shinycssloaders)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(sp)
library(scales)
library(leaflet)
library(plotly)
#devtools::install_github("jbkunst/highcharter")
library(highcharter)
library(DT)
library(mapview)
#devtools::install_github("lgsilvaesilva/mapsBR")
library(mapsBR)

##-- Chamando as funções criadas ----
source("functions/utils.R")
source("functions/plot_functions.R")
cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")

##-- Dados do banco ----
load(file = "dados/dados_gerais.RData")
load(file = "dados/chaves.RData")

anos <- chaves %>% distinct(ANO_ELEICAO) %>% .$ANO_ELEICAO %>% as.numeric() %>% sort
cargos <- chaves %>% mutate(DESCRICAO_CARGO = toupper(DESCRICAO_CARGO)) %>%distinct(CODIGO_CARGO, DESCRICAO_CARGO)
cargos$DESCRICAO_CARGO_EN <- c("PRESIDENT", "SENATOR", "GOVERNOR", "FEDERAL DEPUTY", "STATE DEPUTY", "DISTRITAL DEPUTY", "MAYOR")
chaves <- merge(chaves, cargos, by = c("CODIGO_CARGO", "DESCRICAO_CARGO"))
cargos <- setNames(as.list(cargos$CODIGO_CARGO), cargos$DESCRICAO_CARGO_EN)
partidos <- chaves %>% distinct(SIGLA_PARTIDO) %>% .$SIGLA_PARTIDO %>% sort
estados <- chaves %>% distinct(UF) %>% .$UF
estados <- estados[!is.na(estados)]
municipios_df <- chaves %>% filter(UF == "SP") %>% distinct(COD_MUN_IBGE, NOME_MUNICIPIO) %>% group_by(COD_MUN_IBGE) %>% summarise(NOME_MUNICIPIO = last(NOME_MUNICIPIO))
municipios <- as.list(c(municipios_df$COD_MUN_IBGE, "TODOS MUNICIPIOS"))
names(municipios) <- c(municipios_df$NOME_MUNICIPIO, "All cities")

partidos_cores <- readRDS(file = "dados/cores_partidos.RDS")
load("dados/voronoy/voronoi_est_sp.RData")

##-- Chamando os componentes do header shiny ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))

##-- Chamando os shapes do mapsBR ----
data("regMun")
data("regUF")