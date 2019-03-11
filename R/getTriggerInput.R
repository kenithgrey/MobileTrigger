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
#' @title Get Input From a Mail Trigger Message
#' @description Parses inputs from a message saved when e-mail client run its rules.
#' Parser is looking for UseModel, UseScript, or UseReport follow by a number
#' associated with Model, Script, or Report ID number. For reports and scripts
#' there is also the option to supply comma separated data.
#' @param InputPath string, path to a saved e-mail message from your e-mail client
#' Likely filenames in the [TriggerPath] are modelInput.txt, ScriptInput.txt, or
#' ReportInput.txt
#' @return list,
#' ID, integer ID of the model, script, or report selected.
#' data, data.frame of any supplied inputs.
#'
#' #' @examples
#'
#' #################################
#' # Get E-mail Trigger Input      #
#' #################################
#'
#' # The file is a saved e-mail message from your e-mail client
#' MDL_Input <- MailTriggerInput(InputPath="c:/triggers/modelInput.txt")
#' SCRPT_Input <- MailTriggerInput(InputPath="c:/triggers/ScriptInput.txt")
#' RPT_Input <- MailTriggerInput(InputPath="c:/triggers/ReportInput.txt")
#'

MailTriggerInput <- function(InputPath){

  if(is.null(InputPath)){return(warning("Specify Input Path"))}
  if(!file.exists(InputPath)){return(list(ID=0, data="No Input File In Path"))}


  MessageIn <- scan(file = InputPath, strip.white = T,
                    what="list", sep="\n", quiet = T)


  SelectOperationRow <- grep(pattern = "UseModel|UseScript|UseReport", x = MessageIn)

  if(SelectOperationRow == length(MessageIn)){
    data <- NULL
  }else{
    data <- utils::read.csv2(text=MessageIn, header = T, skip = SelectOperationRow, sep="," )
  }

  ID <- as.numeric(gsub(pattern = '\\D',
                        replacement = "" ,
                        MessageIn[SelectOperationRow]))
  listout <- list(ID=ID, data=data)
  class(listout) <- c("MailTriggerInput", "list")
  return(listout)
}
