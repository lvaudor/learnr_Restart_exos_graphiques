transform_ggelem=function(x){
  if(class(x)[1]=="trans"){x=x$name}
  if(class(x)[1]=="quosures"){x=as.character(x)}
  if(any(stringr::str_detect(class(x),"ggproto"))){x=NULL}
  if(any(stringr::str_detect(class(x),"function"))){x=NULL}
  return(x)
}

identical2=function(x,y){
  x=transform_ggelem(x)
  y=transform_ggelem(y)
  return(identical(x,y))
}
compare_graphics=function(p,g,pass_message){
    # Data
    if(!identical(g$data,p$data)){
      return(fail("Les données que vous avez utilisées ne sont pas celles attendues."))
    }
    # Layers
    if(length(g$layers)!=length(p$layers)){
      return(fail("Votre graphique n'a pas le nombre de couches attendu"))
    }
    
    # General mapping
    g_genmapping=purrr::map(g$mapping, all.vars)
    p_genmapping=purrr::map(p$mapping, all.vars)
    if(!identical(g_genmapping,p_genmapping)){
      return(fail("Les variables utilisées dans le mapping (dans `ggplot()`) ne sont pas celles attendues."))
    }
    
    # Layers
    for (i in 1:length(p$layers)){
      if(class(p$layers[[i]]$geom)[1]!=class(g$layers[[i]]$geom)[1]){
        return(fail("Un des geoms n'est pas du type attendu."))
      }
      if(!identical(p$layers[[i]]$aes_params,g$layers[[i]]$aes_params)){
        return(fail("Un ou plusieurs paramètres fixes dans l'appel aux geoms ne correspond pas aux valeurs attendues."))
      }
      if(class(p$layers[[1]]$position)[1]!=class(g$layers[[1]]$position)[1]){
        return(fail("Le paramètre de position n'est pas celui attendu."))
      }
    }
    
    # Facettes
    if(class(p$facet)[1]!=class(g$facet)[1]){
      return(fail("Vous n'avez pas utilisé le même type de facettes que celui attendu."))
    }
    compare_all_facet_params=purrr::map2_lgl(as.list(p$facet$params),
                                             as.list(g$facet$params),
                                             .f=identical2)
    if(!all(compare_all_facet_params)){
      return(fail("Vous n'avez pas utilisé le paramétrage attendu pour vos facettes"))
    }
    
    # Etiquettes
    if(!identical(p$labels,g$labels)){
      return(fail("Les étiquettes ne sont pas celles attendues."))
    }
    
    # Echelles
    if(length(p$scales$scales)!=length(g$scales$scales)){
      return(fail("J'attendais que vous modifiiez une ou plusieurs échelles..."))
    }
    if(length(p$scales$scales)>0){
       for (i in 1:length(p$scales$scales)){
            compare_all_scales=purrr::map2_lgl(as.list(p$scales$scales[[i]]),
                                               as.list(g$scales$scales[[i]]),
                                               .f=identical2)
            if(!all(compare_all_scales)){
                return(fail("Les échelles ne sont pas paramétrées comme je l'attendais"))
            }
       }
    }
    # Pass
    return(pass(pass_message))
}

