#' @export
#' @title Get Model for Trigger Processing
#' @description Function loads a model from the TRIGGER_PATH]/Models/ folder
#' saved as a *.Rdata file. The Model is then applied to input data collected
#' from an e-mail trigger via the MobileTrigger::MailTriggerInput() function
#' @param ID integer, Model ID value determined from MobileTrigger::ListModels() output
#' when SelectMDL = NULL
#' @param path string, path to the /Models/ folder.
#' @return A model into the environment
#' @examples
#'
#' ################################
#' # Get Model                    #
#' ################################
#' # MODELPATH <- "C:/Triggers/Models/"
#' # MDL <- GetModel(ID = 1, path = MODELPATH)
#' ## To predict use:
#' ## predict(MDL[[1]], newdata) # to get the model use the MDL[[1]]

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
