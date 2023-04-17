
#' quickly generate a nicely formatted flextable object
#'
#' @param tab A dataframe/tibble containing the table information 
#' @param fontsize Fontsize used for the entire table
#' @param caption An optional caption
#' @param footer An optional footer
#' @param digits The number of digits to round to
#' @param percent_vector A vector (e.g. c(0.1,0.2,0.8)) that sums to 1 and identicates how wide each column should be relative to the others
#' @param as_reporter_table Return as report table R6 instance for further processing. By default returns a flextable object
#' @param theme_fun A flextable theme function, by defaut booktab with bold header
#' @param total_width Width of the table in inches. Defaults to regular Word document with 1 inch margins
#'
#' @return a flextable object or a reporter_table object if as_reporter_table=T
#' @export
#'
#' @examples
#' 
#' library(prepkit)
#' tab1<-prep_freq_table(diamonds,cut)
#' report_table(tab1,percent_vector = c(0.5,0.25,0.25),fontsize = 22)
#' 
report_quick_table<-function(tab,fontsize=11,
                      caption=NULL,
                      footer=NULL,
                      digits=2,
                      percent_vector=NULL,
                      as_reporter_table=F,
                      theme_fun=function(x){theme_booktabs(x,bold_header = T)},
                      total_width=6.267717){
  ft<-report_table$new(tab,fontsize = fontsize,percent_vector=percent_vector,theme_fun=theme_fun,
                         total_width = total_width)
  ft$round(digits = digits)
  if(is_null(caption)==F){
    ft$add_caption(caption)
  }
  if(is_null(footer)==F){
    ft$add_footer(footer)
  }
  if(as_reporter_table){
    return(ft)
  } else {
    return(ft$ft)
  }
}
