#########################
# Get Script and Inputs #
#########################

#' @export
ListScripts <- function(path = NULL, SelectSCRIPT=NULL){
###Testing Area###
#path <- "TestEnv/Scripts/"
#SelectSCRIPT <- 1
###End Testing Area

  if(is.null(path)){return("Specify Path to Script Folder")}

  #Get all the scripts in the script folder
  SCRIPTs <-
    list.files(path = path,
               pattern = ".R",
               full.names = T)

  if(length(SCRIPTs) == 0){return("No Scripts in Path")}

  #Make an Empty data frame with the index numbers for later
  XMLSummary_df <- data.frame(`Script ID` = 1:length(SCRIPTs),
                              check.names = F)


  #For each Script See if there is some description information.
  XMLSummary <- lapply(SCRIPTs, function(x){
    fIN <- scan(file = x, what = character(), sep = "\n", quiet = T)
    META_Lines <- grep("<SCRIPT>|</SCRIPT>", x = fIN)
    if(length(META_Lines) == 2)
    {
      META <- fIN[META_Lines[1]:META_Lines[2]]
      META2 <- gsub("#", "", META)
      META3 <- paste0(META2, collapse = "")
    }else{
      META3 <- paste0("<SCRIPT><Title>",
                      basename(x),
                      "</Title></SCRIPT>", collapse = "")
    }

  })

  #Wrap All the desciption info extracted with the <MobileTrigger> tag
  XMLSummary_paste <-
    paste0("<MobileTrigger>",
    paste0(XMLSummary, collapse = ""),
    "</MobileTrigger>")

  #parse as an XML file to dataframe
  XMLSummary_df <- cbind(XMLSummary_df, XML::xmlToDataFrame(XMLSummary_paste))



  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  if(is.null(SelectSCRIPT)){
    TableRows <- nrow(XMLSummary_df)
    knitr::kable(x = XMLSummary_df,format = "html",
               table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells) -> O1

    msg <- paste("<h2 style='color:black;'>Scripts</h2>",
                 paste0(O1, collapse = ""),"<br>",
                 "<h2 style='color:black;'>Example Query</h2>",
                 "UseScript: 1<br><br>",
                 "Input1, Input2<br>",
                 "2,3<br>", sep="\n"
    )

  }else{
    XMLSummary_df <- XMLSummary_df[SelectSCRIPT,]
    TableRows <- nrow(XMLSummary_df)
    knitr::kable(x = XMLSummary_df, format = "html",
                 row.names = F,
                 table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells)

  }

}


