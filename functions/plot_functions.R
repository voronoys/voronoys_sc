##-- + Gráfico de barras ----
bar_plot <- function(data, uf, value_var, group_var){
  ##-- ++ Organizando a base ----
  if(is.null(uf)){
    data <- data %>%
      group_by(NUM_TURNO, CPF_CANDIDATO, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO) %>%
      summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS))
  } else{ 
    data <- data %>% filter(UF == uf) %>%
    group_by(NUM_TURNO, CPF_CANDIDATO, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO) %>%
      summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS))
  }
  
  ##-- ++ Organizando a base ----
  pos_var <- which(names(data) %in% c(value_var, group_var))
  names(data)[pos_var] <- c("group", "value")
  data$label <- paste0(data$group, ": ", percent(data$value/sum(data$value)))
  
  data <- data.frame(data) %>% arrange(desc(value)) %>% mutate(group = factor(group, levels = .$group, labels = .$group))
  
  colors_pal <- partidos_cores[unlist(sapply(data$SIGLA_PARTIDO, function(x) which(x == partidos_cores$partido, arr.ind = T))), "cores"]
  
  ##-- ++ Gráfico ----
  bar <- plot_ly(data,
                 x = ~value, 
                 y = ~rev(group), 
                 color = ~group,
                 colors = colors_pal,
                 hoverinfo = "text",
                 text = ~label,
                 type = 'bar', 
                 orientation = 'h')  %>%
    layout(title = "", 
           xaxis = list(title = "Quantidade de votos"),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           margin = list(l = 3, r = 0, t = 25, b = 45),
           showlegend = T)
  bar
}
##-- + Mapa dos candidatos à presidência por estados ----
mapa_uf <- function(data, poly){
  ##-- ++ Organizando a base ----
  data <- data %>%
    group_by(UF, SIGLA_PARTIDO, NOME_URNA_CANDIDATO) %>%
    summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS, na.rm = T)) %>%
    group_by(UF) %>%
    arrange(desc(QTDE_VOTOS_TOT)) %>%
    filter(QTDE_VOTOS_TOT == max(QTDE_VOTOS_TOT)) %>%
    summarise(NOME_URNA_CANDIDATO = first(NOME_URNA_CANDIDATO), 
              SIGLA_PARTIDO = first(SIGLA_PARTIDO))
  
  ##-- ++ Polígonos ----
  uf_governador_shape <- merge(poly, data)
  uf_governador_shape$SIGLA_PARTIDO <- as.character(uf_governador_shape$SIGLA_PARTIDO)
  
  paleta_col <- partidos_cores %>% filter(partido %in% uf_governador_shape$SIGLA_PARTIDO)
  cores <- paleta_col[unlist(sapply(uf_governador_shape$SIGLA_PARTIDO, function(x) which(x == paleta_col$partido, arr.ind = T))), "cores"]
  
  ##-- ++ Mapa ----
  uf_governador_shape$popup <- paste(uf_governador_shape$SIGLA_PARTIDO, uf_governador_shape$NOME_URNA_CANDIDATO, sep = " - ")
  
  bbox_mun <- bbox(uf_governador_shape)
  map <- leaflet(data = uf_governador_shape) %>%
    addTiles() %>% 
    fitBounds(bbox_mun[1, 1], bbox_mun[2, 1], bbox_mun[1, 2], bbox_mun[2, 2])

  mapa_uf <- map %>%
    addPolygons(stroke = T,
                color = "#000000", 
                group = uf_governador_shape$UF,
                popup = uf_governador_shape$popup,
                label = uf_governador_shape$NOME,
                weight = 0.1,
                smoothFactor = 0.1,
                fillOpacity = 0.7,
                fillColor = cores,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = T)) %>%
    addLegend("bottomright", 
              colors = paleta_col %>% filter(partido %in% uf_governador_shape$SIGLA_PARTIDO) %>% .$cores, 
              labels = paleta_col %>% filter(partido %in% uf_governador_shape$SIGLA_PARTIDO) %>% .$partido,
              opacity = 1)
  mapa_uf
}
##-- + Mapa dos candidatos por município ----
mapa_mun <- function(data, poly, uf = NULL, poly_uf = regUF) {
  ##-- ++ Organizando a base ----
  if (!is.null(uf)) {
    data <- data %>% filter(UF == uf)
  } 
  
  cand_part <- data %>% select(NOME_URNA_CANDIDATO, SIGLA_PARTIDO) %>% distinct()
  
  data <- data %>%
    select(COD_MUN_IBGE, NUM_TURNO, NOME_MUNICIPIO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, QTDE_VOTOS) %>%
    group_by(COD_MUN_IBGE) %>%
    distinct() %>%
    dcast(COD_MUN_IBGE + NOME_MUNICIPIO + NUM_TURNO ~ NOME_URNA_CANDIDATO, value.var = "QTDE_VOTOS", fun.aggregate = sum)
  
  candidatos_nome <- names(data)[-c(1:3)]
  eleito_mun  <- candidatos_nome[apply(data[, -c(1:3)], 1, which.max)]
  votos_total <- apply(data[, -c(1:3)], 1, max, na.rm = T)
  
  data$ELEITO_MUN <- eleito_mun
  data$VOTOS_TOTAL <- votos_total
  
  mun_turno_shape <- poly
  names(mun_turno_shape)[1] <- "COD_MUN_IBGE"
  mun_turno_shape <- merge(mun_turno_shape, data, all.x = T)
  
  if(!is.null(uf)) {
    mun_turno_shape <- subset(mun_turno_shape, UF == uf)
  }
  
  mun_turno_shape$ELEITO_MUN <- as.factor(mun_turno_shape$ELEITO_MUN)
  
  paleta_col <- partidos_cores %>% filter(partido %in% cand_part$SIGLA_PARTIDO) %>% left_join(cand_part, by = c("partido" = "SIGLA_PARTIDO"))
  cores <- paleta_col[unlist(sapply(mun_turno_shape$ELEITO_MUN, function(x) which(x == paleta_col$NOME_URNA_CANDIDATO, arr.ind = T))), "cores"]
  
  ##-- ++ Mapa ----
  mun_turno_shape$popup <- sprintf("<strong>%s</strong><br>Candidate: %s<br><br>Votes: %s<br>", 
                                   mun_turno_shape$NOME_MUNICIPIO, 
                                   mun_turno_shape$ELEITO_MUN, 
                                   mun_turno_shape$VOTOS_TOTAL)
  
  base_toltip <- mun_turno_shape@data %>%
    select(-COD_MUN_IBGE, -UF, -REGIAO, -NOME_MUNICIPIO, -NUM_TURNO, -popup, -VOTOS_TOTAL, -ELEITO_MUN)
  
  toltip <- apply(base_toltip, MARGIN = 1, FUN = function(x) tooltip_map(title = x[1], 
                                                                         vars = names(base_toltip)[-1], 
                                                                         values = x[-1]))
  names(toltip) <- NULL
  
  bbox_mun <- bbox(mun_turno_shape)
  map <- leaflet(data = mun_turno_shape) %>%
    addTiles() %>% 
    fitBounds(bbox_mun[1, 1], bbox_mun[2, 1], bbox_mun[1, 2], bbox_mun[2, 2])
  
  mapa_eleicao_1 <- map%>%
    addPolygons(data = poly_uf, fillOpacity = 0, weight = 0.75, color = "#222222") %>%
    addPolygons(stroke = T,
                color = "black", 
                group = mun_turno_shape$UF,
                popup = toltip,
                label = paste(mun_turno_shape$NOME_MUNICIPIO, mun_turno_shape$UF, sep = " - "),
                weight = 0.3,
                smoothFactor = 0.1,
                fillOpacity = 0.7,
                fillColor = cores,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = T)) %>%
    addLegend("bottomright", 
              colors = paleta_col %>% filter(NOME_URNA_CANDIDATO %in% levels(mun_turno_shape$ELEITO_MUN)) %>% .$cores,
              labels = paleta_col %>% filter(NOME_URNA_CANDIDATO %in% levels(mun_turno_shape$ELEITO_MUN)) %>% .$NOME_URNA_CANDIDATO,
              opacity = 1)
  mapa_eleicao_1
}

tooltip_map <- function(title, vars, values, integer = T){
  head <- sprintf("<strong>%s</strong>", title)
  
  if(integer){
    data_lab <- data.frame(vars, values, stringsAsFactors = F) %>%
      mutate(values = as.integer(values)) %>%
      arrange(desc(values))  
  } else{
    data_lab <- data.frame(vars, values, stringsAsFactors = F)
  }
  
  vars_tab <- apply(data_lab, MARGIN =  1, FUN = function(x) sprintf("<tr>
                                                                        <td><strong>%s</strong></td>
                                                                        <td>%s</td>
                                                                      </tr>", x[1], x[2])) %>% 
    paste(collapse = "")
  
  tab <- paste0(head, "<br><br><table>\n", vars_tab, collapse = "")
  
  return(tab)
}
