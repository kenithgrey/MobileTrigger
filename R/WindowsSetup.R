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
#' @title Setup MobileTrigger Folder Structure and Files
#' @description The heart of the MobileTrigger package. You specify where you
#' want your tigger folder and some e-mail information and this function.
#' This function builds all the scripts you need to get started. At this time,
#' the function works for windows users.
#' @param path string, top level folder called the [TriggerPath]
#' @param SMTP.Server string, SMTP server for your e-mail account
#' @param SMTP.Port integer, port number
#' @param SMTP.User string, user name
#' @param SMTP.Password string, password
#' @param Mail.To string, e-mail address you want MobileTriggers to respond to
#' @param Mail.From string, e-mail address you want MobileTriggers to use to send.
#' @return Sets up a folder and file structure at the [TriggerPath].
#'
#' @examples
#'
#' #################################
#' # Setting of MobileTriggers     #
#' #################################
#' # SetupWindowsTrigger(path="c:/triggers",
#' #                     Mail.To = "Your.Email@mobile.com",
#' #                     Mail.From = "R.Triggers@desktop.com",
#' #                     SMTP.Server = "smtp.server.com",
#' #                     SMTP.Port = 123,
#' #                     SMTP.User = "R.Triggers@desktop.com",
#' #                     SMTP.Password = "1234Password"
#' # )

# Setup Windows Triggers Folder ---------------------------------------------------
SetupWindowsTrigger <- function(path,
                                SMTP.Server,
                                SMTP.Port,
                                SMTP.User,
                                SMTP.Password,
                                Mail.To,
                                Mail.From){

# Create Root --------------------------------------------------------------
  dir.create(path)




# Root: Create MailSetting.R -----------------------------------------------
  fileCon <- file(paste0(path, "/MailSettings.R"))
  SETTINGS <- paste(paste0("SMTP.Server <- '", SMTP.Server, "'"),
                    paste0("SMTP.Port <- '", SMTP.Port, "'"),
                    paste0("SMTP.User <- '", SMTP.User, "'"),
                    paste0("SMTP.Password <- '", SMTP.Password, "'"),
                    paste0("Mail.To <- '", Mail.To, "'"),
                    paste0("Mail.From <- '", Mail.From, "'"),
                    sep = "\n"
                    )
  writeLines(SETTINGS, fileCon)
  close(fileCon)

# Root: Create Models Folder -----------------------------------------------
  ModelPath <- paste0(path, "/", "Models")
  dir.create(ModelPath) #Make The Folder



# Root: #R# Create ListModels.R -----------------------------------------------
  fileCon <- file(paste0(path, "/ListModels.R"))
  SETTINGS <- paste("require(mailR)",
                    "require(MobileTrigger)",

                    paste0("msg<-ListModels(path ='", path, "/Models/')"),
                    paste0("source(file = '", path, "/MailSettings.R', local = T)"),
                    paste0("send.mail(from = Mail.From,
                            to = Mail.To,
                            subject = 'Model List',
                            body = msg,
                            smtp = list(host.name = SMTP.Server,
                                          port = SMTP.Port,
                                          user.name = SMTP.User,
                                          passwd = SMTP.Password,
                                          ssl = TRUE),
                            authenticate = TRUE,
                            send = TRUE,
                            html = T)"),
                    sep="\n"


  )
  writeLines(SETTINGS, fileCon)
  close(fileCon)

# Root: #R# Create RunModels.R -----------------------------------------------
  fileCon <- file(paste0(path, "/RunModels.R"))
  SETTINGS <- paste(
          "require(mailR)",
          "require(MobileTrigger)",
          "# Need to load package for prediction
          require(randomForest)
          require(nnet)",
          "# Setup -------------------------------------------------------------------",
          paste0("MDLpath <- '", path, "/Models/'"),
          paste0("InputPath <- '", path, "/modelInput.txt'"),
          paste0("source(file = '", path, "/MailSettings.R', local = T)"),
          "
          # Read Data and Model -----------------------------------------------------
          Input <- MailTriggerInput(InputPath=InputPath)

          # Load Selected Model -----------------------------------------------------
          MDL <- GetModel(ID = Input$ID, path = MDLpath)

          # Predict -----------------------------------------------------------------
          if(MDL == 'No Models in Path'){
          }else if(!is.null(MDL[[1]]$scaled)){
            if(MDL[[1]]$scaled == T){
              Input$data$Prediction <-
                unlist(predict(MDL[[1]], Input$data)) * MDL[[1]]$outRange + MDL[[1]]$outMin
            }
          }else{
            Input$data$Prediction <- unlist(predict(MDL[1], Input$data))
          }

          # Build Message -----------------------------------------------------------
          msg <- creatMessage(ID=Input$ID,
                              path = MDLpath,
                              outputData = Input$data)

          # Send Message ------------------------------------------------------------
          send.mail(from = Mail.From,
                    to = Mail.To,
                    subject = 'Model Result',
                    body = msg,
                    smtp = list(host.name = SMTP.Server,
                                port = SMTP.Port,
                                user.name = SMTP.User,
                                passwd = SMTP.Password,
                                ssl = TRUE),
                    authenticate = TRUE,
                    send = TRUE,
                    html = T)", sep="\n"
  )
  writeLines(SETTINGS, fileCon)
  close(fileCon)



# Root: #B# Create ListModels.bat ----------------------------------------------
  fileCon <- file(paste0(path, "/ListModels.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/ListModels.R")
  writeLines(CMD, fileCon)
  close(fileCon)

# Root: #B# Create RunModels.Bat -----------------------------------------------------
  fileCon <- file(paste0(path, "/RunModels.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/RunModels.R")
  writeLines(CMD, fileCon)
  close(fileCon)


# Root: Creat Scripts Folder ----------------------------------------------
  ScriptPath <- paste0(path, "/", "Scripts")
  dir.create(ScriptPath) #Make The Folder


# Root: #R# Create ListScripts.r -----------------------------------------------
  fileCon <- file(paste0(path, "/ListScripts.R"))
  SETTINGS <- paste("require(mailR)",
                    "require(MobileTrigger)",

                    paste0("msg<-ListScripts(path ='", path, "/Scripts/')"),
                    paste0("source(file = '", path, "/MailSettings.R', local = T)"),
                    paste0("send.mail(from = Mail.From,
                            to = Mail.To,
                            subject = 'Script List',
                            body = msg,
                            smtp = list(host.name = SMTP.Server,
                                          port = SMTP.Port,
                                          user.name = SMTP.User,
                                          passwd = SMTP.Password,
                                          ssl = TRUE),
                            authenticate = TRUE,
                            send = TRUE,
                            html = T)"),
                    sep="\n"


  )
  writeLines(SETTINGS, fileCon)
  close(fileCon)



# Root: #R# Create RunScripts.r ------------------------------------------------
  fileCon <- file(paste0(path, "/RunScripts.R"))
  SETTINGS <- paste("require(mailR)",
                    "require(MobileTrigger)",
                    paste0("InputPath <- '", path, "/ScriptInput.txt'"),
                    paste0("source(file = '", path, "/MailSettings.R', local = T)"),
                    paste0("SCRIPT <- MailTriggerInput(InputPath=InputPath)"),
                    paste0("ScriptFile <- list.files((path ='", path, "/Scripts/')",
                           ", pattern = '.R', full.names = T)[SCRIPT$ID]"),

                    "tryCatch(
                      source(file = ScriptFile, local = T),
                      error=function(e){",
                    paste0("source(file = '", path, "/MailSettings.R', local = T)"),
                       "send.mail(from = Mail.From,
                                  to = Mail.To,
                                  subject = 'Run Script Error',
                                  body = '<h2>Error Occured:</h2>
                      <ol>
                      <li>Your Script had an error</li>
                      <li>No such Script ID</li>
                      <li>or no Scipt input file</li>
                      </ol>',
                                  smtp = list(host.name = SMTP.Server,
                                              port = SMTP.Port,
                                              user.name = SMTP.User,
                                              passwd = SMTP.Password,
                                              ssl = TRUE),
                                  authenticate = TRUE,
                                  send = TRUE,
                                  html = T)

                      })",
                    sep="\n"
  )
  writeLines(SETTINGS, fileCon)
  close(fileCon)


# Root: #B# Create ListScripts.bat ---------------------------------------------
  fileCon <- file(paste0(path, "/ListScripts.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/ListScripts.R")
  writeLines(CMD, fileCon)
  close(fileCon)

# Root: #B# Create RunScripts.bat ---------------------------------------------
  fileCon <- file(paste0(path, "/RunScripts.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/RunScripts.R")
  writeLines(CMD, fileCon)
  close(fileCon)


# Root: Creat Reports Folder ---------------------------------------------
  ReportPath <- paste0(path, "/", "Reports")
  dir.create(ReportPath) #Make The Folder

# Root: #B# Create ListReports.bat ---------------------------------------------
  fileCon <- file(paste0(path, "/ListReports.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/ListReports.R")
  writeLines(CMD, fileCon)
  close(fileCon)


# Root: #B# Create RunReports.bat ----------------------------------------------
  fileCon <- file(paste0(path, "/RunReports.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/RunReports.R")
  writeLines(CMD, fileCon)
  close(fileCon)

# Root: #R# List Reports --------------------------------------------------
  fileCon <- file(paste0(path, "/ListReports.R"))
  SETTINGS <- paste("require(mailR)",
                    "require(MobileTrigger)",

                    paste0("msg<-ListReports(path ='", path, "/Reports/')"),
                    paste0("source(file = '", path, "/MailSettings.R', local = T)"),
                    paste0("send.mail(from = Mail.From,
                           to = Mail.To,
                           subject = 'Report List',
                           body = msg,
                           smtp = list(host.name = SMTP.Server,
                           port = SMTP.Port,
                           user.name = SMTP.User,
                           passwd = SMTP.Password,
                           ssl = TRUE),
                           authenticate = TRUE,
                           send = TRUE,
                           html = T)"),
                    sep="\n"


  )
  writeLines(SETTINGS, fileCon)
  close(fileCon)




# Root: #R# RunReports.r --------------------------------------------------
  fileCon <- file(paste0(path, "/RunReports.R"))
  SETTINGS <- paste(
  "require(mailR)",
  "require(MobileTrigger)",
  "# Setup -------------------------------------------------------------------",
  paste0("Sys.setenv(RSTUDIO_PANDOC='", Sys.getenv("RSTUDIO_PANDOC") , "')"),
  paste0("InputPath <- '", path, "/ReportInput.txt'"),
  paste0("Attachment <- '", path, "/Reports/MobileTriggerReport.html'"),
  paste0("source(file = '", path, "/MailSettings.R', local = T)"),
  "",
  "REPORT <- MailTriggerInput(InputPath=InputPath)",
  "SelectedReport <-",
  paste0("ListReports(path = '", path , "/Reports/',"),
  "             SelectREPORT = REPORT$ID",
  ")",

  "ReportFile <-",
  "  list.files(",
  paste0("path ='", path, "/Reports/',"),
  "   pattern = '.Rmd',
      full.names = T)[REPORT$ID]

  # Run Report --------------------------------------------------------------
  tryCatch({
    rmarkdown::render(input = ReportFile,
                      output_file = Attachment)
    err.msg <<- 'None'
  },
  error = function(e){err.msg <<- 'There were Errors. Report was not run'}
  )



  # Make Message Content ----------------------------------------------------
  msg <- paste0('<h2>Selected Report</h2>',
                SelectedReport,
                '<h2>OutPut</h2>',
                'Report File Attached if no Errors',
                '<h2>Errors</h2>',
                err.msg,
                collapse = ''
  )


  # Send Message ------------------------------------------------------------
  send.mail(from = Mail.From,
            to = Mail.To,
            subject = 'Requested Report',
            body = msg,
            smtp = list(host.name = SMTP.Server,
                        port = SMTP.Port,
                        user.name = SMTP.User,
                        passwd = SMTP.Password,
                        ssl = TRUE),
            authenticate = TRUE,
            attach.files =  tryCatch(
                             if(err.msg == 'None') Attachment else {NULL},
                             error= function(e){NULL}
                             ),
            send = TRUE,
            html = T)",  sep="\n")

writeLines(SETTINGS, fileCon)
close(fileCon)

# Build Outlook Script ----------------------------------------------------
  fileCon <- file(paste0(path, "/OUTLOOK.txt"))
  OutLookPath = gsub(pattern = "/", replacement = "\\\\", x=path )
  OUTLOOK <- paste(
  "\' Outlook Scripts to Trigger R Scripts\n\n",
  "Sub ListModels(trigger As Outlook.MailItem)",
  paste0("Shell \"", OutLookPath ,"\\ListModels.bat\""),
  "End Sub",
  "Sub RunModels(trigger As Outlook.MailItem)",
  paste0("Folder = \"", OutLookPath, "\\\""),
  "trigger.SaveAs Folder & \"modelInput.txt\", olTXT",
  paste0("Shell \"", OutLookPath ,"\\RunModels.bat\""),
  "End Sub",
  "",
  "\' Outlook Scripts to Trigger R Scripts\n\n",
  "Sub ListScripts(trigger As Outlook.MailItem)",
  paste0("Shell \"", OutLookPath ,"\\ListScripts.bat\""),
  "End Sub",
  "Sub RunScripts(trigger As Outlook.MailItem)",
  paste0("Folder = \"", OutLookPath, "\\\""),
  "trigger.SaveAs Folder & \"ScriptInput.txt\", olTXT",
  paste0("Shell \"", OutLookPath ,"\\RunScripts.bat\""),
  "End Sub",
  "",
  "\' Outlook Scripts to Trigger R Scripts\n\n",
  "Sub ListReports(trigger As Outlook.MailItem)",
  paste0("Shell \"", OutLookPath ,"\\ListReports.bat\""),
  "End Sub",
  "Sub RunReports(trigger As Outlook.MailItem)",
  paste0("Folder = \"", OutLookPath, "\\\""),
  "trigger.SaveAs Folder & \"ReportInput.txt\", olTXT",
  paste0("Shell \"", OutLookPath ,"\\RunReports.bat\""),
  "End Sub",
  sep="\n"
  )
  writeLines(OUTLOOK, fileCon)
  close(fileCon)




# Root: #R# Create StarterMessages ---------------------------------------------------

  fileCon <- file(paste0(path, "/StarterMessages.R"))
  SETTINGS <- paste(
  paste0("source(file = '", path, "/MailSettings.R', local = T)"),
  "#Helper Script to Setup E-mail triggers and mail client
  require(mailR)

  Subjects <- paste('Hey R -',
                    rep(c('Run', 'List'),3),
                    rep(c('Models', 'Scripts', 'Reports'),each=2)
  )

  lapply(Subjects, function(x){
    send.mail(from = Mail.From,
              to = Mail.To,
              subject = x,
              body = 'MobileTrigger Starter Messages',
              smtp = list(host.name = SMTP.Server,
                          port = SMTP.Port,
                          user.name = SMTP.User,
                          passwd = SMTP.Password,
                          ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              html = T)
  })", sep="\n")
  writeLines(SETTINGS, fileCon)
  close(fileCon)

# Root: #B# Create starterMessages.bat -----------------------------------------------------
fileCon <- file(paste0(path, "/starterMessages.bat"))
CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/StarterMessages.R")
writeLines(CMD, fileCon)
close(fileCon)
}
