##############################
# Copyright 2019 Kenith Grey #
##############################

# Copyright Notice --------------------------------------------------------
# This file is part of MobileTrigger.
#
# MobileTrigger is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# MobileTrigger is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MobileTrigger.  If not, see <http://www.gnu.org/licenses/>.

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
#' \donttest{
#' ################################
#' # Get Model                    #
#' ################################
#'   MODELPATH <- "C:/Triggers/Models/"
#'   MDL <- GetModel(ID = 1, path = MODELPATH)
#' ## To predict use:
#'   predict(MDL[[1]], newdata) # to get the model use the MDL[[1]]
#' }

GetModel <- function(ID=NULL, path = NULL){

  pathResult <- .pathTest(path)
  if(pathResult[1] == FALSE){
    stop(pathResult[2])
  }

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
