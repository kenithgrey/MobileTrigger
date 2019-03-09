#' @export
creatMessage <- function(ID, path, outputData){
  CSS_Table <- "style='border: 1px solid black; width: 75%;'"
  CSS_Cells <- "text-align:center; color: black; padding: 5px; border: 1px solid black;"

  x <- ID
  TableRows <- nrow(outputData)
  #print(TableRows)
  O1 <- ListModels(path = path, SelectMDL = x)

  O2 <- knitr::kable(
    x = cbind(`Model ID`=x, outputData),
    format = "html",
    table.attr = CSS_Table)

  O3 <- kableExtra::row_spec(kable_input = O2,
                         row = 0:TableRows,
                         extra_css = CSS_Cells )

  msg <- paste("<h2 style='color:black;'>Model Selected</h2>",
               paste0(O1, collapse = ""),"<br>",
               "<h2 style='color:black;'>Model Results</h2>",
               paste0(O3 , collapse = "")
  )
  return(msg)
}
