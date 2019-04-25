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
#' @param Mail.To string, e-mail address that will trigger your e-mail client (e.g., Outlook or Thunderbird).
#' @param Mail.From string, e-mail address of the mobile device. (simulation purposes)
#' @param Mail.From.SMTP.Settings list, list of SMTP settings to send to mailR::mail.send.
#' @return Sends test messages to your email client. The e-mail will
#' come from the Mail.From address in your mailsetting.R file.
#'
#' @examples
#'
#' #################################
#' # Test Triggers                 #
#' #################################
#'
#' # testTriggers(
#' # TestWhat = "Lists",
#' ## TestWhat = "RunModels",  # Other Test Options
#' ## TestWhat = "RunScripts", # Other Test Options
#' ## TestWhat = "RunReports", # Other Test Options
#' # path = path,
#' # Mail.To = "[desktop.client@home.com]",
#' # Mail.From =  "[your.mobile@gmail.com]",
#' # Mail.From.SMTP.Settings =
#' # list(host.name = "smtp.gmail.com",
#' #      port = 587,
#' #      user.name = "[your.mobile@gmail.com]",
#' #      passwd = '[TVs_With_Knobs]',
#' #      tls = TRUE)
#' # )
#'


testTriggers <- function(TestWhat = "Lists", path = NULL, Mail.To = NULL, Mail.From = NULL, Mail.From.SMTP.Settings = NULL){
  if(any(c(is.null(Mail.To),
           is.null(Mail.From),
           is.null(Mail.From.SMTP.Settings)
           )
         )
     ){
     return(warning("Please define all settings for: Mail.To, Mail.From, Mail.From.SMTP.Settings"))
     }

  MF <- Mail.From
  SMTP <- Mail.From.SMTP.Settings
  MT <- Mail.To
  ###Testing Area
  # path <- "c:/trigger"
  # TestWhat <- "Lists"
  # TestWhat <- "RunModels"
  # TestWhat <- "RunScripts"
  # TestWhat <- "RunReports"
  ###End Testing Area
  if(any(is.null(c(TestWhat, path, Mail.To)))){
    return(warning("Arguments: path and Mail.To must be provided"))
  }

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
              smtp = SMTP,
              authenticate = TRUE,
              send = TRUE,
              html = F)
    Sys.sleep(15)
  })

}
