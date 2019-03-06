observeEvent(input$navbar,{
  if(input$navbar == "home"){
    sendSweetAlert(
      session = session,
      title = "Don't you have much time?",
      text = HTML("Please click here and go first to: <br> 
                  <h3><b><a id='link_info' href='#' class='action-button'><i class='fas fa-arrow-right'></i> Voter profile <i class='fas fa-arrow-left'></i></a></b></h3> 
                  <br> This is our methodological contribution using the voronoi tessellation. Using this methodology, we can see the results of the elections at a more disaggregated level."),
      type = "info", 
      html = TRUE,
      closeOnClickOutside = FALSE
    )
  }
})

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
observeEvent(input$link_info,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "candidatos_perfil_eleitorado")
  closeSweetAlert(session = session)
})