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
#' @title List Available Scripts
#' @description Creates an HTML table for all or selected scripts in the
#' [TRIGGER_PATH]/Scripts/ folder. This table is used as part of a message
#' being sent out to a receiving e-mail client.
#' @param path string, path to the /Scripts/ folder.
#' @param SelectSCRIPT integer, Script ID value determined from
#' MobileTrigger::ListScripts() output when SelectSCRIPT = NULL
#' @return If SelectSCRIPT = NULL: An HTML table of all scripts in the /Scripts/
#' folder. If SelectSCRIPT is a Script ID number from the complete script list it
#' only returns and HTML table with the selected script
#' @examples
#' \donttest{
#' #################################
#' # Get all the Available Scripts #
#' #################################
#'   SCRIPTPATH <- "C:/Triggers/Scripts/"
#'   HTML.Message <- ListScripts(path = SCRIPTPATH)
#'   ## Use the HTML.Message to send table of Scripts with MailR package.
#'
#' #############################
#' # Get Selected  Script      #
#' #############################
#'   HTML.Message <- ListScripts(path = SCRIPTPATH, SelectSCRIPT = 1)
#'   ## Use the HTML.Message to send table of Selected Script with MailR package.
#' }
ListScripts <- function(path = NULL, SelectSCRIPT=NULL){
###Testing Area###
#path <- "TestEnv/Scripts/"
#SelectSCRIPT <- 1
###End Testing Area

  if(is.null(path)){return("Specify Path to Script Folder")}

  #Get all the scripts in the script folder
  SCRIPTs <-
    list.files(path = path,
               pattern = ".R",
               full.names = T)

  if(length(SCRIPTs) == 0){return("No Scripts in Path")}

  #Make an Empty data frame with the index numbers for later
  XMLSummary_df <- data.frame(`Script ID` = 1:length(SCRIPTs),
                              check.names = F)


  #For each Script See if there is some description information.
  XMLSummary <- lapply(SCRIPTs, function(x){
    fIN <- scan(file = x, what = character(), sep = "\n", quiet = T)
    META_Lines <- grep("<SCRIPT>|</SCRIPT>", x = fIN)
    if(length(META_Lines) == 2)
    {
      META <- fIN[META_Lines[1]:META_Lines[2]]
      META2 <- gsub("#", "", META)
      META3 <- paste0(META2, collapse = "")
    }else{
      META3 <- paste0("<SCRIPT><Title>",
                      basename(x),
                      "</Title></SCRIPT>", collapse = "")
    }

  })

  #Wrap All the desciption info extracted with the <MobileTrigger> tag
  XMLSummary_paste <-
    paste0("<MobileTrigger>",
    paste0(XMLSummary, collapse = ""),
    "</MobileTrigger>")

  #parse as an XML file to dataframe
  XMLSummary_df <- cbind(XMLSummary_df, XML::xmlToDataFrame(XMLSummary_paste))



  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  if(is.null(SelectSCRIPT)){
    TableRows <- nrow(XMLSummary_df)
    knitr::kable(x = XMLSummary_df,format = "html",
               table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells) -> O1

    msg <- paste("<h2 style='color:black;'>Scripts</h2>",
                 paste0(O1, collapse = ""),"<br>",
                 "<h2 style='color:black;'>Example Query</h2>",
                 "UseScript: 1<br><br>",
                 "Input1, Input2<br>",
                 "2,3<br>", sep="\n"
    )

  }else{
    XMLSummary_df <- XMLSummary_df[SelectSCRIPT,]
    TableRows <- nrow(XMLSummary_df)
    knitr::kable(x = XMLSummary_df, format = "html",
                 row.names = F,
                 table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells)

  }

}


