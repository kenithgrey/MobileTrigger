#' @export
MailTriggerInput <- function(InputPath){

  if(is.null(InputPath)){return(warning("Specify Input Path"))}

  MessageIn <- scan(file = InputPath, strip.white = T,
                    what="list", sep="\n", quiet = T)


  SelectOperationRow <- grep(pattern = "UseModel|UseScript|UseReport", x = MessageIn)

  if(SelectOperationRow == length(MessageIn)){
    data <- NULL
  }else{
    data <- read.csv2(text=MessageIn, header = T, skip = SelectOperationRow, sep="," )
  }

  ID <- as.numeric(gsub(pattern = '\\D',
                        replacement = "" ,
                        MessageIn[SelectOperationRow]))
  listout <- list(ID=ID, data=data)
  class(listout) <- c("MailTriggerInput", "list")
  return(listout)
}
