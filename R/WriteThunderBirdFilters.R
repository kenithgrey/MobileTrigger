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
#' @title Setup Rule File for ThunderBird & FiltaQuilla
#' @description Function sets up the 6 rules you need to list and run the
#' Models, Scripts, and Reports
#' @param path string, top level folder called the [TriggerPath]
#' @param sent.from string, e-mail address you will be sending your triggers
#' from when you are out of home or office. The mobile e-mail address.
#' @return msgFilterRules.dat on the [TriggerPath].
#'
#' @examples
#'
#' ####################################################
#' # Setting of MobileTriggers With ThunderBird Rules #
#' ####################################################
#' # SetupWindowsTrigger(path="c:/triggers",
#' #                     Mail.To = "Your.Email@mobile.com",
#' #                     Mail.From = "R.Triggers@desktop.com",
#' #                     SMTP.Settings=list(
#' #                         host.name = 'some.smtp.sever.com',
#' #                         port = 587,
#' #                         user.name = 'R.Triggers@desktop.com',
#' #                         passwd = 'password', ssl = TRUE)
#' # )
#'
#' # WriteThunderBirdFilters(path = "c:/triggers",
#' #                         sent.from = "your.email@Mobile.net")


WriteThunderBirdFilters <- function(path = NULL, sent.from = NULL){
unescapedPath <- path
path <- gsub("/", "\\\\", path)
fileCon <- file(paste0(unescapedPath, "/msgFilterRules.dat"))
#print(path)
TH.Filters <-
paste(" version=\"9\"
        logging=\"no\"
        name=\"R Run Scripts\"
        enabled=\"yes\"
        type=\"17\"
        action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#saveMessageAsFile\"",
    paste0("actionValue=\"", path, "\"", collapse = ""  ),
        "action=\"Custom\"
         customId=\"filtaquilla@mesquilla.com#launchFile\"",
    paste0("actionValue=\"", path, "\\RunScripts.bat\"", collapse = ""  ),
    paste0("condition=\"AND (subject,contains,Hey R - Run Scripts) AND (from,contains,", sent.from , ")\"", collapse = ""),
        "name=\"R List Scripts\"
        enabled=\"yes\"
        type=\"17\"
        action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#launchFile\"",
    paste0("actionValue=\"", path, "\\ListScripts.bat\"", collapse = ""  ),
    paste0("condition=\"AND (subject,contains,Hey R - List Scripts) AND (from,contains,", sent.from , ")\"", collapse = ""),
        "name=\"R Run Reports\"
        enabled=\"yes\"
        type=\"17\"
        action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#saveMessageAsFile\"",
    paste0("actionValue=\"", path, "\"", collapse = ""  ),
        "action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#launchFile\"",
    paste0("actionValue=\"", path, "\\RunReports.bat\"", collapse = ""  ),
    paste0("condition=\"AND (subject,contains,Hey R - Run Reports) AND (from,contains,", sent.from , ")\"", collapse = ""),
        "name=\"R List Reports\"
        enabled=\"yes\"
        type=\"17\"
        action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#launchFile\"",
  paste0("actionValue=\"", path, "\\ListReports.bat\"", collapse = ""  ),
  paste0("condition=\"AND (subject,contains,Hey R - List Reports) AND (from,contains,", sent.from , ")\"", collapse = ""),
        "name=\"R Run Models\"
        enabled=\"yes\"
        type=\"17\"
        action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#saveMessageAsFile\"",
  paste0("actionValue=\"", path, "\"", collapse = ""  ),
        "action=\"Custom\"
        customId=\"filtaquilla@mesquilla.com#launchFile\"",
  paste0("actionValue=\"", path, "\\RunModels.bat\"", collapse = ""  ),
  paste0("condition=\"AND (subject,contains,Hey R - Run Models) AND (from,contains,", sent.from , ")\"", collapse = ""),
      "name=\"R List Models\"
      enabled=\"yes\"
      type=\"17\"
      action=\"Custom\"
      customId=\"filtaquilla@mesquilla.com#launchFile\"",
  paste0("actionValue=\"", path, "\\ListModels.bat\"", collapse = ""  ),
  paste0("condition=\"AND (subject,contains,Hey R - List Models) AND (from,contains,", sent.from , ")\"", collapse = ""),
sep="\n")
writeLines(TH.Filters, fileCon)
close(fileCon)
}
