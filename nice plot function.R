
ggplotly_nice <- function(ggplot, shift_legend_to_the_right = F, long_x_ticks = F, long_y_ticks = F){
  
  require(ggplot2)
  require(plotly)
  
  p <- ggplot + theme(strip.background =element_rect(fill="white",
                                           linetype = 0, size = 0),
                      strip.placement = "outside")
  
  #if color and group are dublicated
  if ("colour" %in% names(p$labels) & "group" %in% names(p$labels)){
    if (p$labels$colour == p$labels$group){
      graph <- ggplotly(p, tooltip = c("x", "y", "group"))
    } else{
      graph <- ggplotly(p)
    }
  } else {
    graph <- ggplotly(p)
  }
  
  #if facet exists - correct margins
  if (class(p$facet$super())[1] != "FacetNull"){
    graph$x$layout$margin$b <- 85
    graph$x$layout$margin$l <- 85
    
  }
  
  #correct legends if exist
  names <- names(p$mapping)
  if (length(names[!grepl('x', names)][grepl('y', names)]) > 0 & p$theme$legend.position %in% c('right', 'bottom')){
    
    graph <- graph %>% layout(legend = list(y = 0.5))
    
    #if legend title exists
    if ("legendTitle" %in% names(sapply(graph$x$layout$annotations, function(x) return(x['legendTitle'])))){
      legends <- lapply(graph$x$data, function(x) return(x['legendgroup']))
    
      nbr_of_legends <- length(unique(legends))
    
     nbr_of_legends_to_use <- nbr_of_legends/2 + 2
     height_half <- graph$height/2
     half_length_of_legends <-  nbr_of_legends_to_use * 7
     share_of_half_graph <-   half_length_of_legends / height_half
     y = 0.5 + share_of_half_graph + 0.03
    
    last_annot <- length(graph$x$layout$annotations)
      graph$x$layout$annotations[[last_annot]]$y = y
      #graph$x$layout$annotations[[last_annot]]$x = 1.07  
    }
   
  }
  
  #if legend is to close to graph
  if (shift_legend_to_the_right == T){
    
    graph <- graph %>% layout(legend = list(x = 1.1))
    
    if (!is.null(graph$x$layout$annotations)){
      
    last_annot <- length(graph$x$layout$annotations)
    graph$x$layout$annotations[[last_annot]]$x = graph$x$layout$annotations[[last_annot]]$x*1.1
    
    }
    
  }
  
  #if text of x-axix ticks is too long
  if (long_x_ticks == T) {
    graph$x$layout$margin$b <- 90
  }
  
  #if text of x-axix ticks is too long
  if (long_y_ticks == T) {
    graph$x$layout$margin$l <- 90
  }
  
  return(graph)
}



