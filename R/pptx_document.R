find_reportkit <- function(){
  require(magrittr)
  require(purrr)
  map(.libPaths(),function(current_lib){
    if(any(list.files(current_lib) %in% "reportkit")){
      return(current_lib)
    }
  })%>%purrr::reduce(c)
}
#' R6 Class representing a power point presentation that can modify itself
#' 
#' @export
pptx_document <- R6::R6Class("pptx_document", list(
  presentation = NULL,
  path = NULL,
  ppt_length = 0,
  template = paste0(find_reportkit(),"/reportkit/data/basic_template.pptx"),
  theme = NULL,
  initialize = function(path, theme=dak_theme) {
    self$theme = theme
    # Accept name, name.pptx or filepath/name or filepath/name.pptx
    path <- paste0(fs::path_ext_remove(path),".pptx")
    
    if(file.exists(path)){
      self$path = tools::file_path_as_absolute(path)
      self$presentation = read_pptx(path)
      self$ppt_length = length(self$presentation)
    }else {
      #Erstellen der Datei basierend auf einem Template
      self$presentation = read_pptx(path = self$template)
      self$path = path
      self$presentation <- ph_with(self$presentation, 
                                   value = fpar(
                                     ftext("Content", 
                                           fp_text(font.size = 61.5,
                                                   font.family = "Calibri Light",
                                                   color = theme$header_color,bold = TRUE))
                                   ), location = ph_location(left = 1.078504,top = -0.095 ,width = 15))
      (self$presentation)%>%
        print(self$path)
      self$path = tools::file_path_as_absolute(path)
      self$ppt_length = length(self$presentation)
    }
  },
  add_ggplot = function(gg, header="Titel", align="center", current_slide=F){
    layout<-layout_summary(self$presentation)
    stopifnot(any(str_detect(layout,"Titel und Inhalt|Title and Content")))
    
    all_layouts<-layout%>%pull(layout)
    content_slide_name <- all_layouts[all_layouts %in% c("Titel und Inhalt","Title and Content")][1]
    
    props<-layout_properties(self$presentation,layout = content_slide_name)%>%
      filter(type=="body")%>%
      head(1)
    if(current_slide==F){
      self$presentation<-self$presentation%>%
        add_slide(layout = content_slide_name,master = layout$master[1])%>%
        ph_with(., 
                value = fpar(
                  ftext(header, 
                        fp_text(font.size = 61.5,
                                font.family = "Calibri Light",
                                color = self$theme$header_color,bold = TRUE))
                ), location = ph_location(left = 1.078504,top = -0.095 ,width = 15))
    }
    if(align=="center"){
      self$presentation <- self$presentation%>%
        ph_with(value= gg,location = ph_location_type(type="body"))
    } else if(align=="left"){
      self$presentation <- self$presentation%>%
        ph_with(value= gg,
                location = ph_location(left = props$offx ,top = props$offy ,width = props$cx/2,height = props$cy))
    } else {
      self$presentation <- self$presentation%>%
        ph_with(value= gg,
                location = ph_location(left = props$offx + props$cx/2 ,top = props$offy ,width = props$cx/2,height = props$cy))
    }
    self$ppt_length = length(self$presentation)
  },
  add_flextable = function(tabelle,font_size=30,
                           header="Titel",align="center",current_slide=F,
                           caption=NULL, footer=NULL, digits=2,
                           percent_vector = NULL,
                           theme_fun = theme_booktabs){
    layout<-layout_summary(self$presentation)
    stopifnot(any(str_detect(layout,"Titel und Inhalt|Title and Content")))
    
    all_layouts<-layout%>%pull(layout)
    content_slide_name <- all_layouts[all_layouts %in% c("Titel und Inhalt","Title and Content")][1]
    
    props<-layout_properties(self$presentation,layout = content_slide_name)%>%
      filter(type=="body")%>%
      head(1)
    width <- slide_size(self$presentation)$width-(0.14*slide_size(self$presentation)$width)
    if(align %in% c("left","right")){
      width <- width * 0.5
    }
    ft<-report_quick_table(tabelle%>%
                      rename_all(str_to_sentence),
                    fontsize = font_size,
                    total_width = width,
                    caption = caption,
                    footer = footer,
                    percent_vector = percent_vector,
                    theme_fun = theme_fun)
    
    if(current_slide==F){
      self$presentation<-self$presentation%>%
        add_slide(layout = content_slide_name,master = layout$master[1])%>%
        ph_with(., 
                value = fpar(
                  ftext(header, 
                        fp_text(font.size = 61.5,
                                font.family = "Calibri Light",
                                color = self$theme$header_color,bold = TRUE))
                ), location = ph_location(left = 1.078504,top = -0.095 ,width = 15))
    }
    if(align=="center"){
      self$presentation<-self$presentation%>%
        ph_with(value= ft,location = ph_location_type(type="body"))
    } else if(align=="left"){
      self$presentation<-self$presentation%>%
        ph_with(value= ft,
                location = ph_location(left = props$offx ,top = props$offy ,width = props$cx/2,height = props$cy))
    } else {
      self$presentation<-self$presentation%>%
        ph_with(value= ft,
                location = ph_location(left = props$offx + props$cx/2 ,top = props$offy ,width = props$cx/2,height = props$cy))
    }
    self$ppt_length = length(self$presentation)
    
  },
  open = function(){
    (self$presentation)%>%
      print(self$path)
    browseURL(self$path)
  },
  reload = function(){
    self$presentation = read_pptx(self$path)
    self$ppt_length = length(self$presentation)
  },
  save = function(){
    (self$presentation)%>%
      print(self$path)
  }
))

#' report_pptx
#'
#' @param path either a filepath or name / name.pptx to create a file based on a template in the current workding directory. 
#' Alternatively you may provide a filepath of an existing pptx to read in the existing file.
#'
#' @return an R6 Class called presentation that can modify itself interactively
#' @export
#'
#' @examples
report_pptx<-function(path, theme=dak_theme){
  return(pptx_document$new(path=path, theme=theme))
}