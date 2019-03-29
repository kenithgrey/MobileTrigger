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
#' @title Send message from mobile triggers
#' @description Function is a wrapper for mailR::send.mail() function.
#' @param path string, top level folder called the [TriggerPath]
#' @param body string, body text of the message
#' @param subject string, subject text of the message
#' @param html boolean, use HTML or plain text.
#' @param authenticate boolean, use authentication for SMTP signin
#' @return message to be sent to mobile email client.
#'
#' @examples
#'
#' ####################################################
#' # Setting of MobileTriggers With ThunderBird Rules #
#' ####################################################
#' # the function internals will call your mailsettings
#' # from the mailsettings.R file in your trigger root folder
#'
#' # TriggerMSG(path = 'c:/triggers',
#' #           body = "hello world",
#' #           subject = "Script List",
#' #           html = F, authenticate = T
#' # )


TriggerMSG <-
  function(path, subject = NULL, body = NULL, html = F,
           authenticate = T, debug = F, ...){

    source(paste0(path, "/mailSettings.R", collapse = ""), local = T)
    #print(ls())
    do.call(mailR::send.mail,
            args = list(from=Mail.From, to=Mail.To, html = html,
                        send=T, subject = subject, body = body,
                        smtp = MailRsettings,
                        authenticate = authenticate, debug=debug,
                        ...)
    )
  }
