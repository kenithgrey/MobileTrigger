#' @export
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
