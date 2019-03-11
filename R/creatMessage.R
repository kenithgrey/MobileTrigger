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
# along with ggQC.  If not, see <http://www.gnu.org/licenses/>.



#' @export
#' @title HTML Summary of Model Result
#' @description Function takes i) a seleted model from the TRIGGER_PATH]/Models/ folder
#' and the results and makes an HTML summary for the e-mail message response.
#' @param ID integer, Model ID value determined from MobileTrigger::ListModels() output
#' when SelectMDL = NULL
#' @param path string, path to the /Models/ folder.
#' @param outputData data.frame, table with model, inputs, and outputs.
#' @return A HTML summary for e-mail message response.
#' @examples
#'
#' ################################
#' # creatMessage Example         #
#' ################################
#'
#' # MDLpath <- 'c:/triggers/Models/'
#' # InputPath <- 'c:/triggers/modelInput.txt'
#'
#' # Read Data and Model -----------------------------------------------------
#' # Input <- MailTriggerInput(InputPath=InputPath)
#'
#' # Load Selected Model -----------------------------------------------------
#' # MDL <- GetModel(ID = Input$ID, path = MDLpath)
#' # Predict -----------------------------------------------------------------
#' # if(MDL == 'No Models in Path'){
#' # }else if(!is.null(MDL[[1]]$scaled)){
#' #  if(MDL[[1]]$scaled == T){
#' #    Input$data$Prediction <-
#' #    unlist(predict(MDL[[1]], Input$data)) * MDL[[1]]$outRange + MDL[[1]]$outMin}
#' #   }else{
#' #    Input$data$Prediction <- unlist(predict(MDL[1], Input$data))
#' # }
#'
#' # Build Message -----------------------------------------------------------
#' # msg <- creatMessage(ID = Input$ID,
#' #                     path = MDLpath,
#' #                     outputData = Input$data)

#'  # Send Message ------------------------------------------------------------
#' ## ...
#'

creatMessage <- function(ID, path, outputData){
  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  x <- ID
  TableRows <- nrow(outputData)
  #print(TableRows)
  O1 <- ListModels(path = path, SelectMDL = x)

  O2 <- knitr::kable(
    x = cbind(`Model ID`=x, outputData),
    format = "html",
    table.attr = CSS_Table)

  O3 <- kableExtra::row_spec(kable_input = O2,
                         row = 0:TableRows,
                         extra_css = CSS_Cells )

  msg <- paste("<h2 style='color:black;'>Model Selected</h2>",
               paste0(O1, collapse = ""),"<br>",
               "<h2 style='color:black;'>Model Results</h2>",
               paste0(O3 , collapse = "")
  )
  return(msg)
}
