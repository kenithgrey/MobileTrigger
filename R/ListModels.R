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
#' @import caret
#' @title List Available Models
#' @description Creates an HTML table for all or selected models in the
#' [TRIGGER_PATH]/Models/ folder. This table is used as part of a message
#' being sent out to a receiving e-mail client.
#' @param path string, path to the /Models/ folder.
#' @param SelectMDL integer, Model ID value determined from MobileTrigger::ListModels() output
#' when SelectMDL = NULL
#' @return If SelectMDL = NULL: An HTML table of all models in the /Models/
#' folder. If SelectMDL is a ModelID number from the complete model list it
#' only returns and HTML table with the selected model
#' @examples
#' \donttest{
#' ################################
#' # Get all the Available Models #
#' ################################
#'   MODELPATH <- "C:/Triggers/Models/"
#'   HTML.Message <- ListModels(path = MODELPATH)
#'   ## Use the HTML.Message to send table of Models with MailR package.
#'
#' #############################
#' # Get Selected Model        #
#' #############################
#'   HTML.Message <- ListModels(path = MODELPATH, SelectMDL = 1)
#'   ## Use the HTML.Message to send table of Selected Model with MailR package.
#' }

ListModels <- function(path = NULL, SelectMDL=NULL){

if(is.null(path)){return("Specify Path to Model Folder")}

  MDLS <-
    list.files(path = path,
               pattern = ".RData",
               full.names = T)

if(length(MDLS) == 0){return("No Models in Path")}

  MDLS.env <- new.env()

  temp <- lapply(MDLS, function(x){
    load(file = x, envir = MDLS.env)
  })


  ModelTerms <-
    lapply(ls(MDLS.env), function(x){
      obj <- get(x, envir = MDLS.env)
      #print(caret::predictors(obj))
      terms <- paste0(caret::predictors(obj), collapse = ", ")
      data.frame(Model = x, terms=terms, class=class(obj)[1])
    }
    )

  MDLdf <- do.call(rbind, ModelTerms)
  MDLdf <- cbind(ID=1:nrow(MDLdf), MDLdf)



  #Note to self this get silly with highly paramertized model.
  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  if(is.null(SelectMDL)){
    TableRows <- nrow(MDLdf)
    knitr::kable(x = MDLdf,format = "html",
               table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells) -> O1

    msg <- paste("<h2 style='color:black;'>Models</h2>",
                 paste0(O1, collapse = ""),"<br>",
                 "<h2 style='color:black;'>Example Query</h2>",
                 "UseModel: 1<br><br>",
                 "TermName1, TermName2<br>",
                 "2,3<br>", sep="\n"
    )

  }else{
    MDLdf <- MDLdf[SelectMDL,]
    TableRows <- nrow(MDLdf)
    knitr::kable(x = MDLdf, format = "html",
                 row.names = F,
                 table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells)

  }

}


