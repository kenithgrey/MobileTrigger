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
