#' @export
GetModel <- function(ID=NULL, path = NULL){
  if(is.null(path)){return(warning("Specify Path to Model Folder"))}
  if(is.null(ID)){return(warning("Specify model ID from ModelList()"))}
  MDLS <-
    list.files(path = path,
               pattern = ".RData",
               full.names = T)

  if(length(MDLS) == 0){return("No Models in Path")}

  MDLS.env <- new.env(parent = .GlobalEnv)

  load(file = MDLS[ID], envir = MDLS.env)
  MDL <- as.list.environment(MDLS.env)
  rm(MDLS.env)
  return(MDL)
}
