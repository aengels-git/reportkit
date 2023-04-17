#' R6 Class representing a word document that can modify itself
#' 
#' @export
word_document<-R6::R6Class("word_document", list(
  document = NULL,
  path = NULL,
  initialize = function(path) {
    path <- paste0(fs::path_ext_remove(path),".docx")
    
    if(file.exists(path)){
      self$path = tools::file_path_as_absolute(path)
      self$document<-read_docx(path = path)
    }else {
      #Erstellen der Datei basierend auf einem Template
      self$document = read_docx()
      self$path = path
      (self$document)%>%
        print(self$path)
      self$path = tools::file_path_as_absolute(path)
    }
  },
  add_flextable = function(flextab,align="center"){
    self$document<-(self$document)%>%
      body_add_break()%>%
      body_add_flextable(value = flextab,align=align)
    self$save()
  },
  add_ggplot = function(gg,width = 6.5,height = 4){
    self$document<-(self$document)%>%
      body_add_break()%>%
      body_add_gg(gg,width=width,height=height)
    self$save()
  },
  open = function(){
    pander::openFileInOS(self$path)
  },
  reload = function(){
    self$document<-read_docx(path = self$path)
  },
  save=function(path=NULL){
    if(is_null(path)){
      self$document%>%
        print(self$path)
    }else {
      self$document%>%
        print(path)
      self$path = path
    }
  }
)
)

#' report_word
#'
#' @param path either a filepath or name / name.pptx to create a file based on a template in the current workding directory. 
#' Alternatively you may provide a filepath of an existing pptx to read in the existing file.
#'
#' @return an R6 Class called presentation that can modify itself interactively
#' @export
#'
#' @examples
report_word<-function(path){
  return(word_document$new(path=path))
}
