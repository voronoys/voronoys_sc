output$sobre <- renderUI({
  
  augusto <- includeMarkdown('descricoes/augusto.md')
  
  douglas <- includeMarkdown('descricoes/douglas.md')
  
  felipe <- includeMarkdown('descricoes/felipe.md')
  
  lucas <- includeMarkdown('descricoes/gordoy.md')
  
  luis <- includeMarkdown('descricoes/luis.md')
  
  paste(augusto, br(), douglas, br(), felipe, br(), lucas, br(), luis)
  
})