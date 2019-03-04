##-- + Atualizando abas conforme os cliques ----
observeEvent(input$analise_geral,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "eleicoes")
})
observeEvent(input$analise_partidos,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "partidos")
})
observeEvent(input$analise_candidatos,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "candidatos")
})
observeEvent(input$sobre,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "sobre")
})