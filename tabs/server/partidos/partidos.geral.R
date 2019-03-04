##-- ++ Atualizações dos cargos ----
observeEvent(input$partido_geral_ano,{
  ano <- isolate(input$partido_geral_ano)
  
  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano & DESCRICAO_CARGO_EN != "PRESIDENT") %>%
      distinct(CODIGO_CARGO, DESCRICAO_CARGO_EN)
    
    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$CODIGO_CARGO)
    cargo_default <- input$perfil_candidato_cargo
    
    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }
    
    cargos_list <- setNames(as.list(chaves_sub$CODIGO_CARGO), chaves_sub$DESCRICAO_CARGO_EN)
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "partido_geral_cargo",
                      label = "Position", 
                      choices = cargos_list, 
                      selected = cargo_default)
    
  }
  
}, priority = 1)
##-- + Base prefeitos por partido ----
mapa_pref_partido <- eventReactive(input$partidos_gerar_visualizacoes1, {
  
  ano <- isolate(input$partido_geral_ano)
  cargo <- isolate(input$partido_geral_cargo)
  
  shape <- subset(regMun, !is.na(NOME))
  
  eleitos_map <- dados_gerais %>% filter(CODIGO_CARGO == cargo & ANO_ELEICAO == ano) %>% 
    filter(DESC_SIT_TOT_TURNO == 'ELEITO' & ANO_ELEICAO == ano) %>%
    select(SIGLA_PARTIDO, COD_MUN_IBGE, NOME_MUNICIPIO, ANO_ELEICAO, DESCRICAO_ELEICAO) %>%
    mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO)) %>% 
    group_by(COD_MUN_IBGE) %>%
    mutate(ordem = order(COD_MUN_IBGE)) %>%
    filter(ordem == 1)
  
  names(eleitos_map)[2] <- "COD"
  
  teste <- eleitos_map %>%
    group_by(SIGLA_PARTIDO) %>%
    summarise(n = n()) %>%
    arrange(desc(n))%>%
    mutate(perc = n/ sum(n),
           perca = cumsum(perc),
           partidos = ifelse(perca <= 0.9, SIGLA_PARTIDO, 'Others'))
  
  eleitos_map2 <- merge(eleitos_map, teste , by = "SIGLA_PARTIDO", all.x = T)
  eleitos_map2 <- merge(shape, eleitos_map2, by = "COD", all.x = T)
  
  eleitos_map2@data$partidos <- as.factor(eleitos_map2@data$partidos)
  
  if(cargo == 11){
    pop_up_text <- sprintf("<strong>%s - %s</strong><br>
                       <br>Year: %s <br>",
                           eleitos_map2$SIGLA_PARTIDO,eleitos_map2$NOME, 
                           eleitos_map2$ANO_ELEICAO)
    
    palleta <- tableau_color_pal(palette = 'Tableau 20')(length(levels(eleitos_map2@data$partidos)))
    cor_fator <- colorFactor(palleta,levels = levels(eleitos_map2@data$partidos))
    
    l_mun <- leaflet(eleitos_map2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = regUF, fillOpacity =  0, weight = 0.85, color = "#000000") %>%
      addPolygons(color = ~cor_fator(partidos), 
                  weight = 0.1,
                  popup = pop_up_text,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright", colors = palleta, labels = levels(eleitos_map2@data$partidos),
                opacity = 1)
  }
  
  return(l_mun)
  
})
##-- + Base dados coligações ----
base_coligacoes <- eventReactive(input$partidos_gerar_visualizacoes1, {
  
  ano <- isolate(input$partido_geral_ano)
  cargo <- isolate(input$partido_geral_cargo)
  
  pref <- dados_gerais %>% filter(CODIGO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == 1)
  
  ano_fed <- c(1998, 2002, 2006, 2010, 2014)
  
  if(!(ano %in% ano_fed)){
    pref <- pref %>%
      group_by(SIGLA_PARTIDO, COMPOSICAO_COLIGACAO) %>%
      summarise(tot = n_distinct(COD_MUN_IBGE))
    
    pref$id <- 1:dim(pref)[1]
    
    coligacoes <- strsplit(pref$COMPOSICAO_COLIGACAO, split = " / ")
    coligacoes <- lapply(coligacoes, FUN =  stringr::str_replace_all, pattern = "/", replacement = "")
    coligacoes <- lapply(coligacoes, FUN =  stringr::str_trim, side = "both")
    
    names(coligacoes) <- pref$SIGLA_PARTIDO
    all_coligation <- lapply(seq_along(coligacoes), function(i) data.frame(SIGLA_PARTIDO = names(coligacoes)[[i]],
                                                                           PARTIDO_COLIGACAO = coligacoes[[i]], 
                                                                           N_MUN = pref$tot[i],
                                                                           stringsAsFactors = F))
    
    all_coligation <- rbindlist(all_coligation)
    names(all_coligation) <- c("SIGLA_PARTIDO", "PARTIDO_COLIGACAO", "NUMERO_MUNICIPIOS")
    all_coligation <- all_coligation[!all_coligation$SIGLA_PARTIDO == all_coligation$PARTIDO_COLIGACAO,]
    
    all_coligation <- all_coligation %>%
      group_by(SIGLA_PARTIDO, PARTIDO_COLIGACAO) %>%
      summarise(tot_coligacao = sum(NUMERO_MUNICIPIOS)) %>%
      arrange(desc(tot_coligacao))
    
    ordem <- all_coligation %>%
      group_by(SIGLA_PARTIDO) %>%
      summarise(n = sum(tot_coligacao)) %>%
      arrange(n)%>%
      .$SIGLA_PARTIDO
    
    all_coligation <- ungroup(all_coligation)
    
    all_coligation$SIGLA_PARTIDO <- factor(all_coligation$SIGLA_PARTIDO, levels = ordem) 
    
    all_coligation$PARTIDO_COLIGACAO <- factor(all_coligation$PARTIDO_COLIGACAO, levels = ordem) 
    
    all_coligation$tooltip <- sprintf("%s - %s
                                      Number of coalitions: %s <br>",
                                      all_coligation$SIGLA_PARTIDO,
                                      all_coligation$PARTIDO_COLIGACAO,
                                      all_coligation$tot_coligacao)
  } else {
    pref <- pref %>%
      group_by(SIGLA_PARTIDO, COMPOSICAO_COLIGACAO) %>%
      summarise(tot = n_distinct(UF))
    
    pref$id <- 1:dim(pref)[1]
    
    coligacoes <- strsplit(pref$COMPOSICAO_COLIGACAO, split = " / ")
    coligacoes <- lapply(coligacoes, FUN =  stringr::str_replace_all, pattern = "/", replacement = "")
    coligacoes <- lapply(coligacoes, FUN =  stringr::str_trim, side = "both")
    
    names(coligacoes) <- pref$SIGLA_PARTIDO
    all_coligation <- lapply(seq_along(coligacoes), function(i) data.frame(SIGLA_PARTIDO = names(coligacoes)[[i]],
                                                                           PARTIDO_COLIGACAO = coligacoes[[i]], 
                                                                           N_MUN = pref$tot[i],
                                                                           stringsAsFactors = F))
    
    all_coligation <- rbindlist(all_coligation)
    names(all_coligation) <- c("SIGLA_PARTIDO", "PARTIDO_COLIGACAO", "NUMERO_MUNICIPIOS")
    
    all_coligation <- all_coligation[!(all_coligation$SIGLA_PARTIDO == all_coligation$PARTIDO_COLIGACAO),]
    
    all_coligation <- all_coligation %>%
      group_by(SIGLA_PARTIDO, PARTIDO_COLIGACAO) %>%
      summarise(tot_coligacao = sum(NUMERO_MUNICIPIOS)) %>%
      arrange(desc(tot_coligacao))
    
    ordem <- all_coligation %>%
      group_by(SIGLA_PARTIDO) %>%
      summarise(n = sum(tot_coligacao)) %>%
      arrange(n)%>%
      .$SIGLA_PARTIDO
    all_coligation <- ungroup(all_coligation)
    
    all_coligation$SIGLA_PARTIDO <- factor(all_coligation$SIGLA_PARTIDO, levels = ordem) 
    
    all_coligation$PARTIDO_COLIGACAO <- factor(all_coligation$PARTIDO_COLIGACAO, levels = ordem) 
    
    
    all_coligation$tooltip <- sprintf("%s - %s
                                      Number of coalitions: %s <br>",
                                      all_coligation$SIGLA_PARTIDO,
                                      all_coligation$PARTIDO_COLIGACAO,
                                      all_coligation$tot_coligacao) 
  }
  
  return(all_coligation = all_coligation)
})

output$mapa_partidos_cid <- renderLeaflet({
  mapa_pref_partido()
})

output$heatmap_coligacoes <- renderPlotly({
  
  all_coligation <- base_coligacoes()
  
  palette <- brewer.pal(n = 9, name = 'YlOrRd')
  
  plot_ly(data = all_coligation,
          x = ~PARTIDO_COLIGACAO, 
          y = ~SIGLA_PARTIDO,
          z = ~tot_coligacao,
          type = "heatmap",
          colors = palette,
          hoverinfo = "text",
          text = ~tooltip,
          colorbar = list(title = "Nº coalitions")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = ""))
})