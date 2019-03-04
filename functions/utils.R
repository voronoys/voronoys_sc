##-- Caixas ----
box_voronoys <- function(texto, cor){
  HTML(paste('<div class = "box_voronoys" style = "border:1px solid', 
             cor, '; background-color: ', 
             cor, ';">', 
             h3(texto), '</div>'))
}

tab_voronoys <- function(texto, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', texto, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">
                      <img src="img/',icon,'">
                    </div>
                  </div>
              </div></a>'))
}

build_coligacao <- function(coligacao){
  res <- data.frame()
  
  id <- coligacao$id
  names(id)<- coligacao$SIGLA_PARTIDO
  coli <- strsplit(coligacao$COMPOSICAO_COLIGACAO, split = ",")
  
  for(i in 1:length(coli))
  {
    coli[[i]] <- id[coli[[i]]]
    aux <- data.frame(from = coligacao$SIGLA_PARTIDO[i],
                      to = names(coli[[i]]),
                      UF = coligacao$SIGLA_UF[i])
    res <- rbind(res,aux)
  }
  index <- res$from == res$to
  res <- cbind(res, index)
  res <- res %>% filter(index == "FALSE") %>% select(-index)
  return(res)
}


syncWith <- function(map, groupname) {
  map$dependencies <- c(map$dependencies, minichartDeps())
  
  invokeMethod(map, data = leaflet::getMapData(map), "syncWith", groupname)
}
