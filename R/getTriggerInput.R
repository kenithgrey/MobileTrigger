#' @export
MailTriggerInput <- function(InputPath){

  if(is.null(InputPath)){return(warning("Specify Path to Model Input"))}

  MessageIn <- scan(file = InputPath, strip.white = T,
                    what="list", sep="\n", quiet = T)

  ModelRow <- grep(pattern = "UseModel", x = MessageIn)

  data <- read.csv2(text=MessageIn, header = T, skip = ModelRow, sep="," )

  ID <- as.numeric(gsub(pattern = '\\D',
                        replacement = "" ,
                        MessageIn[ModelRow]))
  listout <- list(ModelID=ID, data=data)
  class(listout) <- c("MailTriggerInput", "list")
  return(listout)
}
