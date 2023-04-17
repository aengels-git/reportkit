#' R6 Class representin´g a flextable that can modify itself
#' 
#' 
#' @export
#' @examples
#' library(prepkit)
#' tab2<-prep_freq_table(diamonds,cut,color)
#' rtab2<-reporter_table$new(tab2,percent_vector = c(0.5,0.25,0.25),fontsize = 22)
#' rtab2$round()
#' rtab2$merge_col_cells(column = "cut",h_lines = T)
#' rtab2$add_label_row(colwidths = c(2,3,3),values = c("Variables","Frequency","Proportions"),align = "left")
#' rtab2$add_caption("Table 1: Table on diamonds")
#' rtab2$add_footer("Footer: this is a table")
#' 
#' 
report_table<-R6::R6Class("report_table", 
                        public = list(
  #' @field tab A dataframe/tibble containing the table information
  tab = NULL,
  
  #' @field modified_tab formatted table (e.g. with rouneded value) 
  modified_tab = NULL,
  
  #' @field ft flextable representation of reporter table 
  ft=NULL,
  
  #' @field nrow number of rows
  nrow=NULL,
  
  #' @field ncols number of columns
  ncol=NULL,
  
  #' @field width_vector with of each columns as a fector
  width_vector=NULL,
  
  #' @field fontsize Fontsize used for the entire table
  fontsize=NULL,
  
  #' @field theme_fun A flextable theme function, by defaut booktab with bold header
  theme_fun=NULL,
  

  #' @description Create a new reporter table 
  #' @param percent_vector a
  #' @param fontsize a
  #' @param theme_fun a
  #' @param total_width a
  #' @return A new reporter table object.
  initialize = function(tab,
                        percent_vector=NULL,
                        fontsize=11,
                        theme_fun=function(x){theme_booktabs(x,bold_header = T)},
                        total_width=6.267717) {
    require(flextable)
    require(officer)
    require(magrittr)
    require(purrr)
    
    self$tab=tab
    self$modified_tab=tab
    self$nrow=dim(tab)[1]
    self$ncol=dim(tab)[2]
    
    if(is_null(percent_vector)){
      width_vector = rep(1/dim(tab)[2],dim(tab)[2])*total_width
    } else {
      width_vector = percent_vector*total_width
    }
    self$width_vector = width_vector
    self$fontsize=fontsize
    self$ft<-flextable(self$modified_tab,cwidth=self$width_vector)
    self$ft=theme_fun(self$ft)
    self$theme_fun=theme_fun
    self$update()
  },
  
  update=function(){
    self$ft=fontsize(self$ft,size = self$fontsize,part = "all")
  },
  
  #' @description
  #' round the table
  #' @param digits the n of digits to round to
  round=function(digits=2){
    self$modified_tab<-self$tab%>%
      mutate_if(is.numeric,function(x){round(x,digits = digits)})
    self$ft<-flextable(self$modified_tab,cwidth=self$width_vector)
    self$ft=self$theme_fun(self$ft)
    self$update()
  },
  
  help = function(){
    browseURL("https://ardata-fr.github.io/flextable-book/static/assets/pdf/cheat_sheet_flextable.pdf")
  },
  
  #' @description add a header row that can be merged across cells
  #' @param colwidths width vector
  #' @param values a
  #' @param align a
  #' @param bold a
  add_label_row= function(colwidths,values,align="center",bold=T){
    if(sum(colwidths)!=self$ncol){
      stop(glue("colwidths' sum must be equal to the number of col_keys ({self$ncol})"))
    }
    self$ft<-add_header_row(self$ft,colwidths = colwidths,values = values)
    self$ft <- bold(self$ft, i = 1, bold = bold, part = "header")
    self$update()
    #Remove header lines
    self$ft<-hline(x =self$ft,part = "header",border = officer::fp_border(color = "#666666", width = 0))
    #Add new Lines:
    self$ft<-hline_top(x =self$ft, part = "header",border = officer::fp_border(color = "#666666", width = 2))
    self$ft<-hline_bottom(x =self$ft, part = "header",border = officer::fp_border(color = "#666666", width = 2))
    self$ft<-align(self$ft,i = 1,align = align,part = "header")
  },
  
  #' @description add a caption 
  #' @param values a caption
  add_caption=function(caption){
    self$ft<-add_header_row(self$ft,values =caption,colwidths = self$ncol)
    self$update()
  },
  
  #' @description add a footer 
  #' @param values a footer
  add_footer=function(footer){
    self$ft<-add_footer_row(self$ft,values =footer,colwidths = self$ncol)
    self$update()
  },
  
  #' @description merge col cells by a variable in tab (should be sorted by that variable!)
  #' @param column a col in tab as a string
  #' @param h_lines bool whether to insert horizontal lines by factor level
  merge_col_cells=function(column,h_lines=T){
    enviroment<-current_env()
    tab<-self$ft
    #Merge cells by colum factor values:
    values<-unique(self$tab[[column]])
    walk(values,function(x){
      col_bool<-self$tab[[column]]==x
      tab<-merge_at(tab,j=which(names(self$tab)==column),i=which(col_bool))
      if(h_lines==T & x!=values[length(values)]){
        tab<-hline(tab,j=NULL,i=which(col_bool)[sum(col_bool)])
      }
      env_poke(env = enviroment,nm="tab",value = tab)
    })
    self$ft<-tab
  },
  #' @description save the file as docx
  #' @param path path to file
  save=function(path){
    if(file.exists(path)){
      word<-read_docx(path = path)
      doc_table<-word%>% body_add_break()%>%body_add_flextable(self$ft)
      print(doc_table, target = path)
    }else {
      save_as_docx(self$ft, path = path,
                   pr_section = prop_section(page_size = page_size(),
                                             page_margins = page_mar()))
    }
  }
)
)
