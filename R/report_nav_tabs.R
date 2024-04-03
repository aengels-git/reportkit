#' ggplot_to_html
#'
#' @param ggplot 
#' @param width 
#' @param height 
#'
#' @return
#' @export
#'
#' @examples
ggplot_to_html<-function(ggplot,width=7,height=3.5){
  return(blastula::add_ggplot(plot_object = ggplot,width=width,height=height))
}

#' report_nav_bar
#'
#' @param titles navbar titles
#' @param bodies list of objects  
#' @param height height of the container
#'
#' @return
#' @export
#'
#' @examples
report_nav_bar <- function(titles,bodies,nav_title="Navigation",
                           full_screen = TRUE,
                           height=4.6, width=NULL,
                           gg_height=5, gg_width=8){
  height <- height *  100
  require(bslib)
  nav_list <- pmap(list(
    titles,
    bodies
  ),function(title,body){
    if(class(body)[1] =="gg"){
      body <- ggplot_to_html(body, height = gg_height, width = gg_width)
    } else if (class(body)[1] %in% c("data.frame","tbl_df")){
      body <- reactable(body)
    } else if (class(body)[1] =="flextable"){
      body <- body%>%
        htmltools_value()
    }
    nav_panel(
      title,
      card(body, height=glue("{height}px"),full_screen = full_screen)
    )
    
  })
  result <- page_navbar(
    title = nav_title,
    !!!nav_list,
    fluid = FALSE,
    fillable=TRUE
  )
  if(is.null(width)){
    return(result)
  } else {
    return(column(width,result))
  }
}

#' report_nav_tabs
#'
#' @param titles navbar titles
#' @param bodies list of objects  
#' @param footers optional footers
#' @param height height of the container
#'
#' @return
#' @export
#'
#' @examples
report_nav_tabs <- function(titles,bodies,footers=NULL,
                            height=4.6, width=NULL,
                            gg_height=5, gg_width=8){
  height <- height *  100
  require(bslib)
  if(is.null(footers)){
    footers <- map(1:length(titles),~NULL)
  }
  nav_list <- pmap(list(
    titles,
    bodies,
    footers
  ),function(title,body,footer){
    if(class(body)[1] =="gg"){
      body <- ggplot_to_html(body, height = gg_height, width = gg_width)
    } else if (class(body)[1] %in% c("data.frame","tbl_df")){
      body <- reactable(body)
    } else if (class(body)[1] =="flextable"){
      body <- body%>%
        htmltools_value()
    }
    if(is.null(footer)){
      nav_panel(
        title,
        card_body(body)
      )
    } else {
      nav_panel(
        title,
        card_body(body),
        card_footer(footer)
      )
    }
    
  })
  result <- navset_card_tab(
    height = height, full_screen = FALSE,
    !!!nav_list
  )
  if(is.null(width)){
    return(result)
  } else {
    return(column(width,result))
  }
  
}


