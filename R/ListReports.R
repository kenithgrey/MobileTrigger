##########################
# Get Reports            #
##########################

#' @export
#' @title List Available Reports
#' @description Creates an HTML table for all or selected reports in the
#' [TRIGGER_PATH]/Reports/ folder. This table is used as part of a message
#' being sent out to a receiving  e-mail client.
#' @param path string, path to the /Reports/ folder.
#' @param SelectREPORT integer, Report ID value determined from
#' MobileTrigger::ListReports() output when SelectREPORT = NULL
#' @return If SelectREPORT = NULL: An HTML table of all scripts in the /Reports/
#' folder. If SelectREPORT is a Report ID number from the complete report list it
#' only returns and HTML table with the selected report.
#' @examples
#'
#' ################################
#' # Get all the Available Reports #
#' ################################
#' # REPORTPATH <- "C:/Triggers/Reports/"
#' # HTML.Message <- ListReports(path = REPORTPATH)
#' ## Use the HTML.Message to send table of Reports with MailR package.
#'
#' #############################
#' # Get Selected  Reports     #
#' #############################
#' # HTML.Message <- ListReports(path = REPORTPATH, SelectREPORT = 1)
#' ## Use the HTML.Message to send table of Selected Script with MailR package.
#'
ListReports <- function(path=NULL, SelectREPORT=NULL){
  ###Testing Area###
  #path <- "TestEnv/Reports/"
  #SelectREPORT <- 1
  ###End Testing Area

  if(is.null(path)){return("Specify Path to Report Folder")}

  #Get all the scripts in the script folder
  REPORTs <-
    list.files(path = path,
               pattern = ".Rmd",
               full.names = T)

  if(length(REPORTs) == 0){return("No Reports in Path")}

  #Make an Empty data frame with the index numbers for later
  Report_Summary_df <-
    data.frame(`Report ID` = 1:length(REPORTs),
                              check.names = F)

  #get the file
  Report_Summary_list <-
    lapply(REPORTs, FUN = function(x){
    CONTENT <- readLines(x)
    HEADER_START_END <- grep("^---", CONTENT)
    HEADER <- CONTENT[HEADER_START_END[1]:HEADER_START_END[2]]
    as.data.frame(yaml::yaml.load(HEADER))
  })

  Report_Summary_df <-
    cbind(Report_Summary_df,
          do.call(plyr::rbind.fill, Report_Summary_list)
          )

  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  if(is.null(SelectREPORT)){
    TableRows <- nrow(Report_Summary_df)
    knitr::kable(x = Report_Summary_df,format = "html",
                 table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells) -> O1

    msg <- paste("<h2 style='color:black;'>Reports</h2>",
                 paste0(O1, collapse = ""),"<br>",
                 "<h2 style='color:black;'>Example Query</h2>",
                 "UseReport: 1<br><br>",
                 sep="\n"
    )

  }else{
    Report_Summary_df <- Report_Summary_df[SelectREPORT,]
    TableRows <- nrow(Report_Summary_df)
    knitr::kable(x = Report_Summary_df, format = "html",
                 row.names = F,
                 table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells)

  }
}
