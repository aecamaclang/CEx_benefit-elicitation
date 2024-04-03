#' ---
#' title: "Create reports for expert review and feedback "
#' author: "Abbey Camaclang"
#' date: "02 Apr 2024"
#' output: github_document
#' ---
#' 
#' Runs a for() loop to render *report.Rmd* and create a report for each expert.  
#' Requires that a Word document template for the report is in the same folder as the R markdown file.
#'
#+ message = FALSE, warning = FALSE
library(markdown)
library(here)

report <- here::here("projects", "TUC_MackenzieCr", "reports") # where reports are saved
rmd <- here::here("projects", "TUC_MackenzieCr", "analysis") # where R markdown file and report template are saved

expvec <- as.character(c(1:10))

for (i in 1:length(expvec)) {
  exp <- expvec[i]
  rmarkdown::render(paste(rmd, "/report.Rmd", sep = ""), params = list(expert = exp), output_format = "word_document", output_file = paste(report, "/exp", exp, ".docx", sep = ""))
}
