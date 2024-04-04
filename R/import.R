#' Functions to manage and aggregate expert estimates for the Conservation Exchange
#' 
#' @description
#' `import` reads in multiple csv files and returns a single table that combines data from all imported files.
#' `clean` adds in a column for expert code, fills in species/functional group/ecotype names, removes 'example' and 'CONFIDENCE' rows, converts B's to Baseline/counterfactual values and X's or blanks to NAs
#' `count` returns a table without the Rating and Comments columns, a 'tidy' version of the table, and a table of the number of expert estimates for each Biodiversity element, Scenario and Estimate type
#' `calculate` calculates the mean values, then determines the mean probability of persistence WITH Action (mean persistence under Counterfactual + mean Benefit), the expected benefit (mean Benefit * mean probability of Success), and expected probability of persistence WITH Action (mean persistence under Counterfactual + mean Expected Benefit)
#'
#' @param string pattern to look for
#' @param filepath location of .csv files to import
#' @param skiplines number of header rows to ignore when importing the .csv files
#' @param numrows number of datarows to import
#' @param x dataframe object to format
#' @param numexp number of expert datasets
#' @param xtable dataframe of combined dataset to count and tidy
#' @param benefit dataframe of estimates of probability of persistence in 'wide' format (Counterfactual and Action estimates in separate columns)
#' @param counts dataframe of the number of estimates for each Biodiversity feature and Estimate type
#' @param feas value for mean probability of success of the project

import <- function(string, filepath, skiplines, numrows) {
  files <- list.files(path = filepath,
                      pattern = string,
                      full.names = T)
  listcsv <- lapply(files,
                    function(x) read.csv(x,
                                         skip = skiplines,
                                         header = T,
                                         nrows = numrows,
                                         as.is = T))
  rawdata <- do.call("rbind", listcsv)
  # rawdata <- rawdata[,1:5]
}

cleandata <- function (x, numrows, numexp) {

  require(naniar)
  require(tidyr)
  require(dplyr)

  # Add column names
  names(x) <- c("Biodiversity",
                "Estimate",
                "Counterfactual",
                "Action",
                "Rating",
                "Comments")

  # Add new column for Expert ID
  tempvec <- c()

  for (i in 1:numexp) {
    tempvec <- c(tempvec, rep(i, times = numrows))
  }

  clean <- x %>%
    mutate(Expert = tempvec, .before = "Biodiversity")

  # Format table
  clean <- clean %>%
  #   mutate(Biodiversity = ifelse(Biodiversity == "", NA, Biodiversity)) %>% # changes blank rows to NA
    fill(Biodiversity) %>% # fills NA rows with species or functional group name
    filter(!grepl("example", Biodiversity)) %>% # remove 'example' rows if present and labelled
    filter(!grepl("CONFIDENCE", Estimate, ignore.case = TRUE)) # remove CONFIDENCE rows if present

  # Replace X's or NAs in Counterfactual column with 'B' if Action estimate is 'B'
  # Replaces B's in Action estimate column with value for baseline/counterfactual estimates 
  # from same row if Counterfactual value is NOT 'X' or NA 
  for (i in 1:nrow(clean)) {
    if (is.na(clean[i,5])==FALSE) {
      if (clean[i,5] == "B" | clean[i,5] == "b"){
        if (clean[i,4] %in% c("X", "X ", "x", "x ")) {
          clean[i,4] <- clean[i,5] # replaces 'X' with 'B'
        } else {
          if (clean[i,4] == "" | is.na(clean[i,4])) {
            clean[i,4] <- clean[i,5] # replaces blanks/NAs with 'B'
          } else {
            clean[i,5] <- clean[i,4] # replaces 'B' with Baseline/counterfactual value
          }
        } 
      }
    }
  }

  # Replace 'X' and blanks with NAs
  out <- as.data.frame(clean) %>%
    replace_with_na(replace = list(Counterfactual = c("X", "X ", "x", "x "), 
                                   Action = c("X", "X ", "x", "x "))) %>% 
    replace_with_na(replace = list(Counterfactual = "", Action = ""))
  
  # Fix estimates entered with percent (%) symbol
  out$Counterfactual <- gsub("\\%", "", out$Counterfactual)
  out$Action <- gsub("\\%", "", out$Action)

  out
}

count <- function(xtable) {

  require(tidyverse)

  x <- xtable %>%
    select(-Rating, -Comments)
  x$Expert <- as_factor(x$Expert)
  x$Biodiversity <- as_factor(x$Biodiversity)
  x$Estimate <- as_factor(x$Estimate)

  # Count the number of expert estimates per species x scenario
  x.long <-
    gather(x,
           key = Scenario,
           value = Value,
           Counterfactual:Action
    )

  temp <- na.omit(x.long)
  temp$Scenario <- as_factor(temp$Scenario)

  x.count <- as.data.frame(table(temp$Biodiversity, temp$Estimate, temp$Scenario))
  names(x.count) <- c("Biodiversity", "Estimate", "Scenario", "Freq")

  x.count <- x.count %>%
    spread(key = Scenario, value = Freq) %>%
    arrange(Biodiversity, Estimate)

  out <- list("table" = x, "tidy" = x.long, "count" = x.count)

}

calculate <- function(benefit, counts, feas) {

  require(tidyverse)

  avg <- aggregate(cbind(Counterfactual, Benefit) ~ Biodiversity + Estimate,
                   data = benefit,
                   FUN = mean, na.action = na.omit)

  results <- avg %>%
    arrange(Biodiversity, Estimate) %>%
    mutate(Persistence_WithAction = Counterfactual + Benefit) %>% # calculate average prob of persistence WITH ACTION
    mutate(ExpBenefit = Benefit*feas) %>% # calculate expected benefit (benefit * feasibility)
    mutate(ExpPersistence_WithAction = Counterfactual + ExpBenefit) %>% # calculate expected performance WITH action (Counterfactual + expected benefit)
    rename(Persistence_Counterfactual = Counterfactual)

  temp.count <- counts %>%
    select(-Counterfactual)

  output <- left_join(results, temp.count, by = c("Biodiversity", "Estimate")) %>%
    rename(Freq = Action)

}
