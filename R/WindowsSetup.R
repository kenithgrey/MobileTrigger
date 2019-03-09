#' @export
# Setup Windows Triggers Folder ---------------------------------------------------
SetupWindowsTrigger <- function(path,
                                SMTP.Server,
                                SMTP.Port,
                                SMTP.User,
                                SMTP.Password,
                                Mail.To,
                                Mail.From){

  #Creat the Root
  dir.create(path)

  ###
  # Make a MailSetting.R file
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

  ###
  # Make a ListModels.R file
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

  ###
  # Make a ListModels.R file
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
          MDL <- GetModel(ID = Input$ModelID, path = MDLpath)

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
          msg <- creatMessage(ID=Input$ModelID,
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


  # --------------------------------
  ModelPath <- paste0(path, "/", "Models")
  dir.create(ModelPath) #Make The Folder

  ###
  # Make a batch file called ListModels.bat
  fileCon <- file(paste0(path, "/ListModels.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/ListModels.R")
  writeLines(CMD, fileCon)
  close(fileCon)

  ###
  # Make a batch file called RunModels.bat
  fileCon <- file(paste0(path, "/RunModels.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/RunModels.R")
  writeLines(CMD, fileCon)
  close(fileCon)

  # --------------------------------
  ScriptPath <- paste0(path, "/", "Scripts")

  dir.create(ScriptPath) #Make The Folder

  ###
  # Make a batch file called ListModels.bat
  fileCon <- file(paste0(path, "/ListScripts.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/ListScripts.R")
  writeLines(CMD, fileCon)
  close(fileCon)

  ###
  # Make a batch file called RunModels.bat
  fileCon <- file(paste0(path, "/RunScripts.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/RunScripts.R")
  writeLines(CMD, fileCon)
  close(fileCon)

  # --------------------------------
  ReportPath <- paste0(path, "/", "Report")

  dir.create(ReportPath) #Make The Folder

  ###
  # Make a batch file called ListModels.bat
  fileCon <- file(paste0(path, "/ListReports.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/ListReports.R")
  writeLines(CMD, fileCon)
  close(fileCon)

  ###
  # Make a batch file called RunModels.bat
  fileCon <- file(paste0(path, "/RunReports.bat"))
  CMD <- paste0(R.home("bin"), "/" , "Rscript.exe ", path, "/RunReports.R")
  writeLines(CMD, fileCon)
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
  "End Sub", sep="\n"
  )
  writeLines(OUTLOOK, fileCon)
  close(fileCon)


}

