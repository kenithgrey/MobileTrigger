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
#' @title Test Mobile Triggers
#' @description Tests to make sure communication between R and
#' your e-mail client is working as expected with MobileTriggers.
#' @param TestWhat string, options: "Lists", "RunModels", "RunScripts", "RunReports"
#' @param path string, top level folder called the [TriggerPath]
#' @param Mail.To string, e-mail address of machine running R and mobileTriggerts
#' @param Mail.From string, e-mail address your mail client is set to respond to.
#' @return Sends test messages to your email client.
#'
#' @examples
#'
#' #################################
#' # Test Triggers                 #
#' #################################


testTriggers <- function(TestWhat = "Lists", path = NULL,  Mail.From = NULL, Mail.To = NULL){
  MF <- Mail.From
  MT <- Mail.To
  ###Testing Area
  # path <- "c:/trigger"
  # TestWhat <- "Lists"
  # TestWhat <- "RunModels"
  # TestWhat <- "RunScripts"
  # TestWhat <- "RunReports"
  ###End Testing Area
  if(any(is.null(c(TestWhat, path, Mail.From, Mail.To)))){
    return(warning("Arguments: path, Mail.From, and Mail.To must be provided"))
  }

  #initial Settings
  SMTP.Server <- character()
  SMTP.Port <- numeric()
  SMTP.User <- character()
  SMTP.Password <- character()




  source(paste0(path,"/MailSettings.R"), local = T)

  #Message Setup
  Mail.df <- data.frame(
    Subjects = c("Hey R - List Models", "Hey R - List Scripts",
                 "Hey R - List Reports",
                 "Hey R - Run Models", "Hey R - Run Models",
                 "Hey R - Run Scripts", "Hey R - Run Scripts",
                 "Hey R - Run Reports", "Hey R - Run Reports"),
    Messages = c(" ", " ", " ",
                 "UseModel:1 \n\n Units_of_A,Units_of_B \n\n 1,2",
                 "UseModel:4 \n\n Units_of_A,Units_of_B \n\n\ 1,2",
                 "UseScript:1 \n\n Age,Name \n\n 5,William", "UseScript:2",
                 "UseReport:1", "UseReport:2"), stringsAsFactors = F
  )

  if(TestWhat == "Lists"){
    selected <- 1:3
  }else if(TestWhat == "RunModels"){
    selected <- 4:5
  }else if(TestWhat == "RunScripts"){
    selected <- 6:7
  }else if(TestWhat == "RunReports"){
    selected <- 8:9
  }


  lapply(selected, function(x){
    print(Mail.df$Subjects[x])
    print(Mail.df$Messages[x])
    mailR::send.mail(from = MF,
              to = MT,
              subject = Mail.df$Subjects[x],
              body = Mail.df$Messages[x],
              smtp = list(host.name = SMTP.Server,
                          port = SMTP.Port,
                          user.name = SMTP.User,
                          passwd = SMTP.Password,
                          ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              html = F)
    Sys.sleep(15)
  })

}
