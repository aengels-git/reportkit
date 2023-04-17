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


#' prep_nav_tabs
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
report_nav_tabs <- function(titles,bodies,footers=NULL,height=4.6, width=NULL){
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
      body <- ggplot_to_html(body)
    } else if (class(body)[1] %in% c("data.frame","tbl_df")){
      body <- reactable(body)
    }
    if(is.null(footer)){
      nav(
        title,
        card_body_fill(body)
      )
    } else {
      nav(
        title,
        card_body_fill(body),
        card_footer(footer)
      )
    }
    
  })
  result <- navs_tab_card(
    height = height, full_screen = FALSE,
    !!!nav_list
  )
  if(is.null(width)){
    return(result)
  } else {
    return(column(width,result))
  }
  
}


