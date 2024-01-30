.onLoad <- function(libname, pkgname){
  
  # DAK Specific theme:
  dak_theme <- rlang::new_environment()
  dak_theme$name <- "DAK"
  dak_theme$header_color <- "#FF6501"

  rlang::env_poke(env = rlang::global_env(),
                  nm = "dak_theme",value = dak_theme)
  print(find_reportkit())

}