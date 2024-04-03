#' ---
#' title: "Compile and manage expert estimates"
#' author: "Abbey Camaclang"
#' date: "27 Mar 2024"
#' output: github_document
#' ---
#'   
#' This script:  
#' 1) Compiles expert estimates from multiple .csv files into single table.   
#' 2) Performs data cleaning as needed (e.g., removing example rows or 
#' empty rows, standardizing variable names, converting B's into Counterfactual 
#' values and X's into NAs).  
#'   
#+ warning = FALSE, message = FALSE 
# Load packages
library(here)
library(tidyverse)
library(naniar)

# Specify input and output filepaths
success <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "success")
sar <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "sar")
group <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "group")
ecotype <- here("projects", "TUC_MackenzieCr", "data", "initial", "input", "ecotype")

derived <- here("projects", "TUC_MackenzieCr", "data", "initial", "derived")
results <- here("projects", "TUC_MackenzieCr", "analysis", "initial")

# Load custom functions
code <- here("R")
source(paste0(code, "/import.R")) 

# Specify parameters
nexp <- 10 # number of experts
nsar <- 21 # number of SAR in the spreadsheets
ngrp <- 13 + 13 + 11 + 11 # number of ecological groups
neco <- 4 # number of ecotypes

# Specify number of data rows to import from .csv files (after skiplines)
# sarrows <- (nsar*4) # (4 rows [Best Guess, Lowest, Highest, Confidence] per species)
sarrows <- ((nsar+3)*4) # NOTE for TUC_MackenzieCr, 2 sets of blank rows (= 8 rows) (rows were hidden), and the example row need to be removed.
grprows <- (ngrp*4)
ecorows <- (neco*4)

#' Probability of project success

# Combine multiple .csv files into single data frame
feasdata <- import(paste0("*.csv"), filepath = paste(success, "/", sep=""), skiplines = 9,
                   numrows = 6, numexp = nexp) # includes the Notes/Comments box
# skiplines can be a bit finicky - needs some trial & error)

names(feasdata) <- c("Estimate", "Value")

# NOTE for TUC_MackenzieCr project, final row for exp03 and 10 doesn't import 
# manual fix:
feasdata <- add_row(feasdata, Estimate = NA, Value = NA, .before = 18)
feasdata <- add_row(feasdata, Estimate = NA, Value = NA)

# Format data frame
# Add column for expert ID
tempvec <- c()

for (i in 1:nexp) {
  tempvec <- c(tempvec, rep(i, times = 6)) # 'times' should equal the number of data rows to import
}

feasdata <- feasdata %>%
  select(Estimate, Value) %>%
  mutate(Expert = tempvec, .before = "Estimate")

# Rename Estimate types
feasdata$Estimate[which(str_detect(feasdata$Estimate, "LOWEST") == 1)] <- "LOWEST"
feasdata$Estimate[which(str_detect(feasdata$Estimate, "HIGHEST") == 1)] <- "HIGHEST"
feasdata$Estimate[which(str_detect(feasdata$Estimate, "BEST") == 1)] <- "BEST GUESS"

write.csv(feasdata, paste(derived, "/Feas_estimates.csv", sep = ""), row.names = FALSE)


#' Species-at-Risk

# Use custom function to combine multiple .csv files into single data frame
sardata <- import(paste0("*.csv"), filepath = paste(sar, "/", sep=""),
                  skiplines = 15, numrows = sarrows, numexp = nexp) 

# Check that all rows have been read
if (nrow(sardata) != sarrows*nexp) {
  warning("Unexpected number of rows in the SAR table")
}

# Format data
# Use custom function to clean up and format data frame
sarclean <- cleandata(sardata, numrows = sarrows, numexp = nexp)

# Renaming to remove population info from species name, for plotting
sarclean$Biodiversity[which(str_detect(sarclean$Biodiversity, "Bull Trout") == 1)] <- "Bull Trout"
sarclean$Biodiversity[which(str_detect(sarclean$Biodiversity, "Rainbow Trout") == 1)] <- "Rainbow Trout"
sarclean$Biodiversity[which(str_detect(sarclean$Biodiversity, "Western Toad") == 1)] <- "Western Toad"
sarclean$Biodiversity[which(str_detect(sarclean$Biodiversity, "Wolverine") == 1)] <- "Wolverine"
sarclean$Biodiversity[which(str_detect(sarclean$Biodiversity, "Western Bumble Bee") == 1)] <- "Western Bumble Bee"

# NOTE: for the TUC_MackenzieCr project, the example rows must be removed, 
# along with two 'blank' set of rows (= 2 species) that come after Bull Trout
# Creates vector rows (index) of blank rows, then removes them out
index1 <- c(1:3, 7:12) 
indexall <- index1
for (i in 2:nexp) {
  temp <- index1 + ((nsar+3)*3)*(i-1)
  indexall <- c(indexall, temp)
}

sarclean <- sarclean %>%
  filter(!row_number() %in% indexall)

# Quick check to see if rows match up
if (nrow(sarclean) != nsar*3*nexp) {
  warning("Unexpected number of rows in the SAR table")
}

write.csv(sarclean, paste(derived, "/SAR_Estimates.csv", sep = ""), row.names = FALSE)


#' Functional Groups

# Use custom function to combine multiple .csv files into single data frame
grpdata <- import(paste0("*.csv"), filepath = paste(group, "/", sep=""),
                  skiplines = 15, numrows = grprows-2, numexp = nexp) 
# NOTE two of the 'CONFIDENCE' rows (after row 127 and 170 of Excel file) is missing from spreadsheet

grpdata <- grpdata[,1:6] # remove 'Example species' column

# Use custom function to clean up and format data frame
grpclean <- cleandata(grpdata, numrows = grprows-2, numexp = nexp)

# Fix (standardize) group names
grpclean$Biodiversity[which(grpclean$Biodiversity == "Freshwater\nReptiles and amphibians\nSnakes and Lizards")] <-
  "Freshwater\nReptiles and amphibians\nSnakes and lizards"
grpclean$Biodiversity[which(grpclean$Biodiversity == "Wetland\nReptiles and amphibians\nSnakes and Lizards")] <- 
  "Wetland\nReptiles and amphibians\nSnakes and lizards"

# Fix non-standard values
# For TUC_MackenzieCr project, experts can:
# 1) use "F" to indicate that estimate is the same as their Freshwater estimate 
# for the same taxon/functional group (exp04)
# 2) use "C" to indicate that estimate is the same as their Coniferous Forest
# estimate for the same taxon/functional group (exp04) 
# In addition, exp06 entered "C" in the Expertise Rating column instead of Counterfactual/Action

# Copy "C" from Rating column to Counterfactual and Action columns for all 3 Estimates)
rateC <- which(str_detect(grpclean$Rating, "C") == 1)
for (i in rateC) {
  grpclean$Counterfactual[i:(i+2)] <- "C" # (where i = LOWEST, i+1 = HIGHEST, i+2 = BEST GUESS)
  grpclean$Action[i:(i+2)] <- "C"
}

# Split Biodiversity column
grp.temp <- grpclean %>%
  separate(Biodiversity, c("Ecotype", "Taxa", "FuncGroup"), sep = "\\n")

# Replace F's with the corresponding Freshwater estimate from same expert (exp04)
baseF <- which(grpclean$Counterfactual == "F")
for (i in baseF) {
  # Get estimate ID variables
  taxon <- grp.temp$Taxa[i] 
  fg <- grp.temp$FuncGroup[i] 
  expert <- grp.temp$Expert[i] 
  est <- grp.temp$Estimate[i]
  # Find corresponding "F" (Freshwater) estimate
  idx <- which(grp.temp$Expert == expert & 
                 grp.temp$Ecotype == "Freshwater" & 
                 grp.temp$Taxa == taxon & 
                 grp.temp$FuncGroup == fg & 
                 grp.temp$Estimate == est)
  # Replace values using idx
  grp.temp$Counterfactual[i] <- grp.temp$Counterfactual[idx]
  grp.temp$Action[i] <- grp.temp$Action[idx]
}

# Replace C's with the corresponding Coniferous forest estimate from same expert (exp06)
baseC <- which(grpclean$Counterfactual == "C")
for (i in baseC) {
  # Get estimate ID variables
  taxon <- grp.temp$Taxa[i] 
  fg <- grp.temp$FuncGroup[i] 
  expert <- grp.temp$Expert[i] 
  est <- grp.temp$Estimate[i]
  # Find corresponding "C" (Coniferous) estimate
  idx <- which(grp.temp$Expert == expert & 
                 grp.temp$Ecotype == "Coniferous Forest" & 
                 grp.temp$Taxa == taxon & 
                 grp.temp$FuncGroup == fg & 
                 grp.temp$Estimate == est)
  # Replace values using idx
  grp.temp$Counterfactual[i] <- grp.temp$Counterfactual[idx]
  grp.temp$Action[i] <- grp.temp$Action[idx]
}

# Re-unite Biodiversity columns
grpclean <- unite(data = grp.temp, 
                  col = Biodiversity, c("Ecotype", "Taxa", "FuncGroup"), 
                  sep = "\n")

write.csv(grpclean, paste(derived, "/FuncGrp_Estimates.csv", sep = ""), row.names = FALSE)


#' Ecotypes

# Use custom function to combine multiple .csv files into single data frame
ecodata <- import(paste0("*.csv"), filepath = paste(ecotype, "/", sep=""),
                  skiplines = 15, numrows = ecorows, numexp = nexp) # one of the CONFIDENCE rows is missing from spreadsheet

# Check that all rows have been read
if (nrow(ecodata) != ecorows*nexp) {
  warning("Unexpected number of rows in the SAR table")
}

# Use custom function to clean up and format data frame
ecoclean <- cleandata(ecodata, numrows = ecorows, numexp = nexp)

write.csv(ecoclean, paste(derived, "/Ecotype_Estimates.csv", sep = ""), row.names = FALSE)
