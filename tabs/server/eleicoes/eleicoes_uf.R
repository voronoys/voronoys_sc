##-- Atualizações dos filtros ----
##-- + Atualizações dos cargos ----
observeEvent(input$eleicoes_ano_uf,{
  ano <- isolate(input$eleicoes_ano_uf)
  
  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano & !(DESCRICAO_CARGO_EN %in% c("MAYOR", "CITY COUNCILOR", "STATE DEPUTY", "FEDERAL DEPUTY", "DISTRITAL DEPUTY", "PRESIDENT"))) %>%
      distinct(CODIGO_CARGO, DESCRICAO_CARGO_EN)

    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$CODIGO_CARGO)
    cargo_default <- input$eleicoes_cargo_uf
    
    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }
    
    cargos_list <- setNames(as.list(chaves_sub$CODIGO_CARGO), chaves_sub$DESCRICAO_CARGO_EN)
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "eleicoes_cargo_uf",
                      label = "Position", 
                      choices = cargos_list, 
                      selected = cargo_default)
    
  }
  
}, priority = 1)
##-- + Atualizações dos turnos ----
observeEvent(c(input$eleicoes_ano_uf, 
               input$eleicoes_cargo_uf),{
                 
                 ano <- isolate(input$eleicoes_ano_uf)
                 cargo <- isolate(input$eleicoes_cargo_uf)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo)
                   
                   ##-- Setando o cargo default
                   turnos <- unique(chaves_sub$NUM_TURNO)
                   turno_default <- input$eleicoes_turno_uf
                   
                   if(!(turno_default %in% turnos) | length(turnos) == 0){
                     turno_default <- "1º round"
                   }
                   
                   if(length(turnos) == 0){
                     turnos <- ""
                   } else{
                     turnos <- paste0(turnos, "º round")
                   }
                   
                   ##-- Atualizando os turnos ----
                   updatePickerInput(sessio = session,
                                     inputId = "eleicoes_turno_uf", 
                                     label = "Round", 
                                     choices = turnos, 
                                     selected = turno_default)
                 }
                 
               }, priority = 2)
##-- + Atualizações dos estados ----
observeEvent(c(input$eleicoes_ano_uf, 
               input$eleicoes_cargo_uf, 
               input$eleicoes_turno_uf),{
                 
                 ano <- isolate(input$eleicoes_ano_uf)
                 cargo <- isolate(input$eleicoes_cargo_uf)
                 turno <- isolate(input$eleicoes_turno_uf)
                 turno <- ifelse(turno != "1º round", "2", "1")
                 
                 if(!is.null(turno)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & NUM_TURNO == turno)
                   ##-- Setando o estado default
                   estados <- levels(factor(x = sort(unique(chaves_sub$UF)),
                                            levels = sort(unique(chaves_sub$UF))))
                   estado_default <- input$eleicoes_estado_uf
                   
                   if(!(estado_default %in% estados)){
                     estado_default <- "AC"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "eleicoes_estado_uf",
                                     label = "State", 
                                     choices = estados, 
                                     selected = estado_default)  
                 }
                 
               }, priority = 3)
##-- Reactive para os dados ----
dados_eleicao_geral_uf <- reactive({
  ##-- + Inputs ----
  ano <- input$eleicoes_ano_uf
  cargo <- input$eleicoes_cargo_uf
  turno <- input$eleicoes_turno_uf
  turno <- ifelse(turno != "1º round", "2", "1")
  
  ##-- + Selecionando os dados ----
  dados <- dados_gerais %>% filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo & NUM_TURNO == turno)
  
  return(dados)
})
##-- Reactive para gerar as visualizações ----
graficos_eleicao_geral_uf <- eventReactive(input$eleicoes_gerar_visualizacoes_uf, {
  dados <- dados_eleicao_geral_uf()
  
  cod_uf <- input$eleicoes_estado_uf
  if(cod_uf == "All states") cod_uf <- NULL
  
  graficos <- list()
  names(regUF)[c(1, 3)] <- c("UF", "REG")
  graficos[[1]] <- mapa_uf(data = dados, poly = regUF)
  graficos[[2]] <- mapa_mun(data = dados, poly = regMun, uf = cod_uf)
  graficos[[3]] <- bar_plot(data = dados, uf = cod_uf, value_var = "QTDE_VOTOS_TOT", group_var = "NOME_URNA_CANDIDATO")
  
  names(graficos) <- c("mapa_uf_uf", "mapa_mun_uf", "bar_plot_uf")
  
  return(graficos)
  
})
##-- Mapa dos candidatos à presidência por estados ----
output$mapa_uf_geral_uf <- renderLeaflet({
  graficos_eleicao_geral_uf()$mapa_uf_uf
})
##-- Mapa dos candidatos municipais ----
output$mapa_mun_geral_uf <- renderLeaflet({
  graficos_eleicao_geral_uf()$mapa_mun_uf
})
##-- Gráfico de barras com o percentual de votos por candidato ----
output$barras_geral_uf <- renderPlotly({
  graficos_eleicao_geral_uf()$bar_plot_uf
})