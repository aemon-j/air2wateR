#'@title Return the path to a template of current NML files
#'
#'@description
#'This returns a path to a directory with example NML files for running FLake and DAT files which contain the model forcing data.
#'
#'@keywords methods
#'
#'@author
#'Tadhg Moore
#'@examples
#'\dontrun{
#' template_files()
#'}
#'
#'@export
air2water_template_files <- function(){
  return(system.file('extdata/', package=packageName()))
}
