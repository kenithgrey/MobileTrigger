#########################
# Get Models and Inputs #
#########################

#' @export
ListModels <- function(path = NULL, SelectMDL=NULL){

if(is.null(path)){return("Specify Path to Model Folder")}

  MDLS <-
    list.files(path = path,
               pattern = ".RData",
               full.names = T)

if(length(MDLS) == 0){return("No Models in Path")}

  MDLS.env <- new.env()

  temp <- lapply(MDLS, function(x){
    load(file = x, envir = MDLS.env)
  })


  ModelTerms <-
    lapply(ls(MDLS.env), function(x){
      obj <- get(x, envir = MDLS.env)
      #print(caret::predictors(obj))
      terms <- paste0(caret::predictors(obj), collapse = ", ")
      data.frame(Model = x, terms=terms, class=class(obj)[1])
    }
    )

  MDLdf <- do.call(rbind, ModelTerms)
  MDLdf <- cbind(ID=1:nrow(MDLdf), MDLdf)



  #Note to self this get silly with highly paramertized model.
  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  if(is.null(SelectMDL)){
    TableRows <- nrow(MDLdf)
    knitr::kable(x = MDLdf,format = "html",
               table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells) -> O1

    msg <- paste("<h2 style='color:black;'>Models</h2>",
                 paste0(O1, collapse = ""),"<br>",
                 "<h2 style='color:black;'>Example Query</h2>",
                 "UseModel: 1<br><br>",
                 "TermName1, TermName2<br>",
                 "2,3<br>", sep="\n"
    )

  }else{
    MDLdf <- MDLdf[SelectMDL,]
    TableRows <- nrow(MDLdf)
    knitr::kable(x = MDLdf, format = "html",
                 row.names = F,
                 table.attr=CSS_Table) ->.;
    kableExtra::row_spec(kable_input = .,
                         row = 0:TableRows,
                         extra_css = CSS_Cells)

  }

}


