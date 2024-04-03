#' ---
#' title: "Convert Excel to CSV files"
#' author: "Abbey Camaclang"
#' date: "27 Mar 2024"
#' output: github_document
#' ---
#'   
#' Converts Excel spreadsheet with multiple tabs into .csv files, one per tab in the spreadsheet.  
#' Requires that all expert spreadsheets are saved in the same folder and no other Excel files are in the folder. 
#'   
#+ warning = FALSE, message = FALSE 
# Load packages
library(here)
library(readxl)

# Folder path where raw Excel files are located
raw <- here("projects", "TUC_MackenzieCr", "data", "initial", "raw")

# Folder paths where .csv files will be saved
success <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "success")
sar <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "sar")
group <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "group")
ecotype <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "ecotype")

# Create a vector of Excel filenames to read
files.to.read = list.files(raw, pattern="xlsx")

# Convert each worksheet into separate .csv for each data type
ws <- data.frame(matrix(c(2:5, success, sar, group, ecotype), ncol = 2))
names(ws)<-c("sheet", "path")
ws$sheet <- as.numeric(ws$sheet)

for (s in 1:nrow(ws)) {
  fp <- ws$path[s]
  lapply(files.to.read, function(f) {
    df = read_excel(paste(raw, "/", f, sep = ""), sheet = ws$sheet[s])
    write.csv(df, paste(fp, "/", gsub("xlsx", "csv", f), sep = ""), row.names=FALSE)
  })
}
