#' R6 Class representing a word document that can modify itself
#' 
#' @export
word_document<-R6::R6Class("word_document", list(
  document = NULL,
  path = NULL,
  initialize = function(path=NULL) {
    require(flextable)
    require(officer)
    require(magrittr)
    require(purrr)
    if(is_null(path)){
      self$document<-read_docx()
    }else{
      self$document<-read_docx(path = path)
      self$path=path
    }
  },
  add_ft = function(flextab){
    self$document<-(self$document)%>%
      body_add_break()%>%
      body_add_flextable(value = flextab)
  },
  add_ggplot = function(gg,default_theme=T){
    if(default_theme==T){
      gg <- gg+theme_bw(base_size = 15)
    }
    self$document<-(self$document)%>%
      body_add_break()%>%
      body_add_gg(gg)
  },
  open = function(){
    pander::openFileInOS(self$path)
  },
  reload = function(){
    self$document<-read_docx(path = self$path)
  },
  save=function(path=NULL){
    if(is_null(path)){
      stopifnot(!is_null(self$path))
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

