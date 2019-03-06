sobre <- tabPanel(title = "About us", 
                  value = "sobre", 
                  br(), hr(),
                  
                  includeHTML(rmarkdown::render('descricoes/augusto.Rmd')), br(),
                  
                  includeHTML(rmarkdown::render('descricoes/douglas.Rmd')), br(),
                  
                  includeHTML(rmarkdown::render('descricoes/felipe.Rmd')), br(),
                  
                  includeHTML(rmarkdown::render('descricoes/gordoy.Rmd')), br(),
                  
                  includeHTML(rmarkdown::render('descricoes/luis.Rmd'))
)