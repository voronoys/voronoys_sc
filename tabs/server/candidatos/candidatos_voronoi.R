observeEvent(input$perfil_candidato_voronoi_metodologia,{
  showModal(
    modalDialog(
      size = "l",
      title = "REFERENCE",
      tags$iframe(style = "height:600px; width:100%; scrolling = yes", src = "pdf/voronoi_data_linkage.pdf"),
      footer = tagList(modalButton("Ok"))
    ))
})

##-- + Atualizações dos filtros ----
##-- ++ Atualizações dos turnos ----
observeEvent(c(input$perfil_candidato_voronoi_ano, 
               input$perfil_candidato_voronoi_cargo),{
                 
                 ano <- isolate(input$perfil_candidato_voronoi_ano)
                 cargo <- isolate(input$perfil_candidato_voronoi_cargo)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO_EN == cargo)
                   
                   ##-- Setando o cargo default
                   turnos <- unique(chaves_sub$NUM_TURNO)
                   turno_default <- input$perfil_candidato_voronoi_turno
                   
                   if(!(turno_default %in% turnos)){
                     turno_default <- "1º round"
                   }
                   
                   turnos <- paste0(turnos, "º round")
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(sessio = session,
                                     inputId = "perfil_candidato_voronoi_turno", 
                                     label = "Round", 
                                     choices = turnos, 
                                     selected = turno_default)
                 }
                 
               }, priority = 2)

##-- ++ Atualizações dos partidos ----
observeEvent(c(input$perfil_candidato_voronoi_ano, 
               input$perfil_candidato_voronoi_cargo, 
               input$perfil_candidato_voronoi_turno),{
                 
                 ano <- isolate(input$perfil_candidato_voronoi_ano)
                 cargo <- isolate(input$perfil_candidato_voronoi_cargo)
                 turno <- isolate(input$perfil_candidato_voronoi_turno)
                 turno <- ifelse(turno == "1º round", "1", "2")
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO_EN == cargo & NUM_TURNO == turno)
                   
                   ##-- Setando o cargo default
                   partidos <- levels(factor(x = c("All parties", sort(unique(chaves_sub$SIGLA_PARTIDO))),
                                             levels = c("All parties", sort(unique(chaves_sub$SIGLA_PARTIDO)))))
                   partido_default <- input$perfil_candidato_voronoi_partido
                   
                   if(!(partido_default %in% partidos)){
                     partido_default <- "All parties"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "perfil_candidato_voronoi_partido",
                                     label = "Party", 
                                     choices = partidos, 
                                     selected = partido_default)  
                 }
                 
               }, priority = 3)

##-- ++ Atualizações dos candidatos ----
observeEvent(c(input$perfil_candidato_voronoi_ano, 
               input$perfil_candidato_voronoi_cargo, 
               input$perfil_candidato_voronoi_turno, 
               input$perfil_candidato_voronoi_partido,
               input$perfil_candidato_voronoi_estado),{
                 
                 ano <- isolate(input$perfil_candidato_voronoi_ano)
                 cargo <- isolate(input$perfil_candidato_voronoi_cargo)
                 turno <- isolate(input$perfil_candidato_voronoi_turno)
                 turno <- ifelse(turno == "1º round", "1", "2")
                 partido <- isolate(input$perfil_candidato_voronoi_partido)
                 estado <- isolate(input$perfil_candidato_voronoi_estado)
                 
                 if(!is.null(ano)){
                   
                   if(!is.null(cargo)){
                     
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO_EN == cargo & NUM_TURNO == turno)
                     
                     if(!is.null(partido)){
                       
                       if(partido != "All parties"){
                         chaves_sub <- chaves_sub %>%
                           filter(SIGLA_PARTIDO == partido)
                       }
                       
                       if(estado != "All states"){
                         if(cargo != "PRESIDENT"){
                           chaves_sub <- chaves_sub %>%
                             filter(UF == estado)  
                         }
                       }
                       
                       ##-- Atualizando os candidatos ----
                       chaves_sub <- chaves_sub %>% arrange(NOME_CANDIDATO)
                       candidatos <- as.list(chaves_sub$CPF_CANDIDATO)
                       names(candidatos) <- chaves_sub$NOME_CANDIDATO
                       
                       updatePickerInput(session = session,
                                         inputId = "perfil_candidato_voronoi_cpf", 
                                         label = "Candidate", 
                                         choices = candidatos, 
                                         selected = NULL)
                     }
                   }
                 }
                 
               }, priority = 5)

base_votos <- eventReactive(input$perfil_candidato_voronoi_gerar_visualizacoes, {
  COD_MUN <- isolate(input$perfil_candidato_voronoi_municipio)
  PARTIDO <- isolate(input$perfil_candidato_voronoi_partido)
  TURNO <- isolate(input$perfil_candidato_voronoi_turno)
  TURNO <- ifelse(TURNO == "1º round", "1", "2")
  
  #votos_aux <- votos %>% filter(NUM_TURNO == TURNO)
  
  mun <- municipios_df %>%
    filter(COD_MUN_IBGE == COD_MUN) %>%
    .$NOME_MUNICIPIO
  
  mun <- iconv(mun, from = "UTF-8", to = "ASCII//TRANSLIT")
  
  if(COD_MUN != "TODOS MUNICIPIOS"){
    
    votos_aux <- voronoi_sp %>% 
      filter(cidade == mun) %>% 
      st_transform(4326)
    
  } else{
    votos_aux <- voronoi_sp %>% st_transform(4326)
  }
  
  aux2 <- as.data.frame(votos_aux) %>% select(PARTIDO)
  aux2 <- aux2[PARTIDO] %>% unclass()
  aux2 <- aux2[[1]] %>% `*`(100) %>% 
    round(2) 
  
  return(list(percentuais = aux2, sf_secoes = votos_aux))
})

mapas_voronois <- eventReactive(input$perfil_candidato_voronoi_gerar_visualizacoes, {
  
  ##-- Voronoi ----
  PARTIDO <- input$perfil_candidato_voronoi_partido
  
  cor_partido <- partidos_cores %>% filter(partido == PARTIDO) %>% .$cores
  
  base_votos_df <- base_votos()
  aux2 <- base_votos_df$percentuais
  sf_secoes <- base_votos_df$sf_secoes
  
  max_votos <- max(aux2)/100
  paleta1 <- colorNumeric(palette = colorRampPalette(colors = c("white", cor_partido))(100), 
                          domain = c(0, max_votos), reverse = F, na.color = cor_partido)
  
  base_tooltip <- sf_secoes %>% select_(.dots = c("cidade", "pop", PARTIDO, "renda", "homens", "analf", "brancos")) %>%
    as.data.frame() %>%
    select(-geometry) %>%
    mutate(pop = round(pop),
           partido = paste0(round(100*get(PARTIDO), 2), " %"),
           renda = paste0("R$ ", round(renda, 2)),
           homens = paste0(round(100*homens, 2), " %"),
           analf = paste0(round(100*analf, 2), " %"),
           brancos = paste0(round(100*brancos, 2), " %")) %>%
    select_(.dots = c("cidade", "pop", "partido", "renda", "homens", "analf", "brancos")) 
  
  base_tooltip[[PARTIDO]] <- NULL
  
  toltip <- apply(base_tooltip, MARGIN = 1, FUN = function(x) tooltip_map(title = paste(x[1], "-", x[2]), 
                                                                          vars = c("Votes", "Income", "Men", "Illiteracy", "White"), 
                                                                          values = x[-c(1:2)], integer = F))
  
  bbox_mun <- st_bbox(sf_secoes)
  names(bbox_mun) <- NULL
  
  l1 <- leaflet(data = sf_secoes) %>%
    fitBounds(bbox_mun[1], bbox_mun[2], bbox_mun[3], bbox_mun[4]) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addPolygons(color = "#222222", weight = 0.1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.7,
                fillColor = ~paleta1(get(PARTIDO)),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                #label = ~cidade,
                popup = toltip,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto")) %>% 
    addLegend("bottomright", pal = paleta1, values = seq(0, 1, length.out = 100), 
              opacity = .9, 
              title = "Votes", 
              labFormat = labelFormat(suffix = "%", digits = 4, transform = function(x) x*100))
  
  ##-- Variáveis ---- 
  
  variavel_label <- input$perfil_candidato_voronoi_variavel
  
  variavel <- switch(variavel_label,
                     "Income" = "log_renda",
                     "Income (categories)" = "renda_cat",
                     "% white"= "brancos", 
                     "% not white"= "nao_brancos", 
                     "% illiteracy" = "analf", 
                     "% women" = "mulheres",
                     "% men" = "homens")
  
  ## Criando variáveis se necessário
  
  if(variavel == "log_renda"){
    sf_secoes <- sf_secoes %>%
      mutate(log_renda = log(renda))
  }
  
  if(variavel == "renda_cat"){
    brks <- c(0,  255,  765, 1912, 3825)
    lbl <- LETTERS[5:1]
    
    sf_secoes <- sf_secoes %>%
      mutate(renda_cat = findInterval(x = renda, vec = brks))
    
    sf_secoes$renda_cat <- factor(sf_secoes$renda_cat, levels = 1:5, labels = LETTERS[5:1], ordered = T)
    
    paleta1 <- colorFactor(palette = colorRampPalette(c("white", cor_partido))(4), sf_secoes$renda_cat)
  } 
  
  if(!(variavel %in% c("log_renda", "renda_cat"))){
    sf_secoes[, variavel] <- sf_secoes[, variavel, drop = T]
    
    base_tooltip <- sf_secoes %>% select_(.dots = c("cidade", "pop", variavel)) %>%
      as.data.frame() %>%
      select(-geometry) %>%
      mutate(pop = round(pop),
             variavel = paste0(round(100*get(variavel), 2), " %")) %>%
      select_(.dots = c("cidade", "pop", "variavel")) 
    
    toltip <- apply(base_tooltip, MARGIN = 1, FUN = function(x) tooltip_map(title = paste(x[1], "-", x[2]), 
                                                                            vars = variavel_label, 
                                                                            values = x[-c(1:2)], integer = F)) 
    
  } else{
    
    base_tooltip <- sf_secoes %>% select_(.dots = c("cidade", "pop", "renda")) %>%
      as.data.frame() %>%
      select(-geometry) %>%
      mutate(pop = round(pop),
             renda = paste0("R$", round(renda, 2))) %>%
      select_(.dots = c("cidade", "pop", "renda")) 
    
    toltip <- apply(base_tooltip, MARGIN = 1, FUN = function(x) tooltip_map(title = paste(x[1], "-", x[2]), 
                                                                            vars = c("Income"), 
                                                                            values = x[-c(1:2)], integer = F))
    
  }
  
  if(variavel != "renda_cat"){
    
    min_var <- min(sf_secoes[, variavel, drop = T])
    max_var <- max(sf_secoes[, variavel, drop = T])
    
    paleta1 <- colorNumeric(palette = colorRampPalette(colors = c("white", cor_partido))(100), 
                            domain = c(min_var, max_var), reverse = F, na.color = cor_partido)
  }
  
  l2 <- leaflet(data = sf_secoes) %>%
    fitBounds(bbox_mun[1], bbox_mun[2], bbox_mun[3], bbox_mun[4]) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addPolygons(color = "#222222", weight = 0.1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.7,
                fillColor = ~paleta1(get(variavel)),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                popup = toltip,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"))
  
  if(variavel == "log_renda"){
    l2 <- l2 %>%
      addLegend("bottomright", pal = paleta1, values = ~get(variavel),
                labFormat = labelFormat(prefix = "R$ ", digits = 2, transform = function(x) exp(x)),
                title = variavel_label,
                opacity = .9)
    
  } 
  
  if(variavel == "renda_cat"){
    l2 <- l2 %>%
      addLegend("bottomright", pal = paleta1, values = ~get(variavel),
                title = variavel_label,
                opacity = .9)
  }
  
  if(!(variavel %in% c("log_renda", "renda_cat"))){
    l2 <- l2 %>% 
      addLegend("bottomright", pal = paleta1, values = ~get(variavel),
                title = variavel_label,
                labFormat = labelFormat(suffix = "%", digits = 4, transform = function(x) x*100),
                opacity = .9)
  }
  
  return(list(l1 = l1, l2 = l2))
})

output$mapa_voronoi <- renderUI({
  mapas <- mapas_voronois()
  sync(mapas$l1, mapas$l2, ncol = 2, sync = list(c(1:2)), sync.cursor = TRUE, no.initial.sync = FALSE)
  
})