#' ---
#' title: "Prepare data tables for plotting"
#' author: "Abbey Camaclang"
#' date: "27 Mar 2024"
#' output: github_document
#' ---

#' This script prepares data tables for plotting.  
#' Two plots will be created for each Biodiversity type  
#' 1) boxplots of the best guess, lowest, and highest estimates for
#' the Counterfactual and With Action scenarios;  
#' 2) pointrange plots showing the best guess, lowest, and highest
#' estimates of each Expert for the Counterfactual and With Action, along
#' with the corresponding averaged estimates.  
#' Comments from experts are also compiled in a table following the plots.  
#'   
#' Requires output files (_Estimates_tidy.csv and _results.csv) created by *AggregateEstimates.R*  
#' Outputs .rds files used to create reports for expert review
#'
#+ message = FALSE, warning = FALSE
# Load packages
library(tidyverse)
library(here)

# Specify paths to subfolders within current working (R project) directory
derived <- here("projects", "TUC_MackenzieCr", "data", "initial", "derived") # where compiled data tables should be saved
results <- here("projects", "TUC_MackenzieCr", "analysis", "initial") # where results of analysis should be saved

#' Custom function for reading in and formatting tidy version of combined expert data
read_tidydata <- function(filename) {
  long <- read.csv(filename)

  strat.levels <- unique(long$Scenario)
  bio.levels <- unique(long$Biodiversity)
  expcode <- unique(long$Expert)
  est.levels <- c("LOWEST", "BEST GUESS", "HIGHEST")

  long$Expert <- factor(long$Expert, levels = expcode)
  long$Biodiversity <- factor(long$Biodiversity, levels = bio.levels)
  long$Scenario <- factor(long$Scenario, levels = strat.levels)
  long$Estimate <- factor(long$Estimate, levels = est.levels)

  long <- na.omit(long)
}

#' Custom function for reading in averaged estimates and reformatting for the plots
read_results <- function(filename) {
  temp.result <- read.csv(filename)

  result <- temp.result %>%
    select(Biodiversity, Estimate, Persistence_Counterfactual, Persistence_WithAction) %>%
    rename(Counterfactual = Persistence_Counterfactual, Action = Persistence_WithAction) %>%
    pivot_longer(Counterfactual:Action, names_to = "Scenario", values_to = "Value" ) %>%
    pivot_wider(names_from = Estimate, values_from = Value)

  result$Biodiversity <- as_factor(result$Biodiversity)
  result$Scenario <- as_factor(result$Scenario)

  result

}
#' Read in and format data  
#' Species-at-risk data
# Get individual estimates and reformat table
sarfile <- paste0(derived, "/SAR_Estimates_tidy.csv")
sar.long <- read_tidydata(sarfile)

sar.long$Value <- na_if(sar.long$Value, "B") # where no value is provided for Counterfactual persistence
sar.long$Value <- as.numeric(sar.long$Value)

sar.levels <- levels(sar.long$Biodiversity)
newlevels <- c(sar.levels[1:10], sar.levels[21], sar.levels[11:20]) # for TUC_MackenzieCr, moving EVGR to after RUBL
sar.long$Biodiversity <- factor(sar.long$Biodiversity, levels = newlevels)

sar.wide <- sar.long %>%
  pivot_wider(names_from = Estimate, values_from = Value)

# Split table for plotting over multiple pages
# May require some trial and error to figure out best arrangement on page
sar1 <- c(newlevels[1:11])
sar2 <- c(newlevels[12:21])

sar.wide1 <- sar.wide %>%
  filter(Biodiversity %in% sar1)
sar.wide2 <- sar.wide %>%
  filter(Biodiversity %in% sar2)

# Get averaged data and split as above
sarresult <- paste0(results, "/SAR_results.csv")
sar.avg <- read_results(sarresult)

sar.avg1 <- sar.avg %>%
  filter(Biodiversity %in% sar1)
sar.avg2 <- sar.avg %>%
  filter(Biodiversity %in% sar2)

#' Functional groups data
# Get individual estimates
grpfile <- paste0(derived, "/FuncGrp_Estimates_tidy.csv")
grp.long <- read_tidydata(grpfile)

grp.long$Value <- na_if(grp.long$Value, "B") # where no value is provided for Counterfactual persistence
grp.long$Value <- as.numeric(grp.long$Value)

# Reorder functional groups by taxa and reformat table
grp.levels <- levels(grp.long$Biodiversity)

grp.split <- data.frame(do.call(rbind, strsplit(grp.levels, '\\n'))) # split group names into 3 columns
names(grp.split) <- c("Ecotype", "Taxa", "FunctionalGroup")
grp.split$Ecotype <- as_factor(grp.split$Ecotype)
grp.split$Taxa <- as_factor(grp.split$Taxa)

grp.split <- grp.split %>%
  arrange(Taxa) %>% # rearrange by taxonomic group
  unite("Biodiversity", Taxa:FunctionalGroup, sep = "\n") # recombine taxa and functional group into single name
fg.levels <- unique(grp.split$Biodiversity) # get new factor levels, now in order by taxa

grp.long$Biodiversity <- as.character(grp.long$Biodiversity)
grp.byeco <- grp.long %>%
  separate(Biodiversity,
           into = c("Ecotype", "FunctionalGroup"),
           sep = "\\n", extra = "merge") %>% # for subsetting by ecotype
  rename(Biodiversity = FunctionalGroup)
grp.byeco$Ecotype <- as_factor(grp.byeco$Ecotype)
grp.byeco$Biodiversity <- factor(grp.byeco$Biodiversity, levels = fg.levels) # reorder using new factor levels

# Split table by ecotype for plotting over multiple pages. 
# May require some trial and error to figure out best arrangement
grp.freshwater <- grp.byeco %>%
  filter(Ecotype == "Freshwater") %>%
  select(-Ecotype)
freshwater.wide1 <- grp.freshwater[grep("Fish|Reptiles", grp.freshwater$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
freshwater.wide2 <- grp.freshwater[grep("Birds", grp.freshwater$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
freshwater.wide3 <- grp.freshwater[grep("Mammals|plants", grp.freshwater$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)

grp.wetland <- grp.byeco %>%
  filter(Ecotype == "Wetland") %>%
  select(-Ecotype)
wetland.wide1 <- grp.wetland[grep("Fish|Reptiles", grp.wetland$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
wetland.wide2 <- grp.wetland[grep("Birds", grp.wetland$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
wetland.wide3 <- grp.wetland[grep("Mammals|plants", grp.wetland$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)

grp.coniferous <- grp.byeco %>%
  filter(Ecotype == "Coniferous Forest") %>%
  select(-Ecotype)
coniferous.wide1 <- grp.coniferous[grep("Reptiles", grp.coniferous$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
coniferous.wide2 <- grp.coniferous[grep("Birds", grp.coniferous$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
coniferous.wide3 <- grp.coniferous[grep("Mammals|plants", grp.coniferous$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)

grp.mixedf <- grp.byeco %>%
  filter(Ecotype == "Mixed Forest") %>%
  select(-Ecotype)
mixedf.wide1 <- grp.mixedf[grep("Reptiles", grp.mixedf$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
mixedf.wide2 <- grp.mixedf[grep("Birds", grp.mixedf$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)
mixedf.wide3 <- grp.mixedf[grep("Mammals|plants", grp.mixedf$Biodiversity),] %>%
  pivot_wider(names_from = Estimate, values_from = Value)

# Get averaged data and split as above
grpresult <- paste0(results, "/FuncGrp_results.csv")
grp.avg <- read.csv(grpresult) ###ABBEY -> figure out why this is using read.csv instead of read_results()

grp.avg <- grp.avg %>%
  unite("Biodiversity", Taxa:FuncGroup, sep = "\n") %>%
  select(Ecotype, Biodiversity, Estimate, Persistence_Counterfactual, Persistence_WithAction) %>%
  rename(Counterfactual = Persistence_Counterfactual, Action = Persistence_WithAction) %>%
  pivot_longer(Counterfactual:Action, names_to = "Scenario", values_to = "Value" ) %>%
  pivot_wider(names_from = Estimate, values_from = Value)

grp.avg$Biodiversity <- factor(grp.avg$Biodiversity, levels = fg.levels)
grp.avg$Scenario <- as_factor(grp.avg$Scenario)
grp.avg$Ecotype <- as_factor(grp.avg$Ecotype)


freshwater.avg <- grp.avg %>%
  filter(Ecotype == "Freshwater") %>%
  select(-Ecotype)
fresh.avg1 <- freshwater.avg[grep("Fish|Reptiles", freshwater.avg$Biodiversity),]
fresh.avg2 <- freshwater.avg[grep("Birds", freshwater.avg$Biodiversity),]
fresh.avg3 <- freshwater.avg[grep("Mammals|plants", freshwater.avg$Biodiversity),]


wetland.avg <- grp.avg %>%
  filter(Ecotype == "Wetland") %>%
  select(-Ecotype)
wet.avg1 <- wetland.avg[grep("Fish|Reptiles", wetland.avg$Biodiversity),]
wet.avg2 <- wetland.avg[grep("Birds", wetland.avg$Biodiversity),]
wet.avg3 <- wetland.avg[grep("Mammals|plants", wetland.avg$Biodiversity),]


coniferous.avg <- grp.avg %>%
  filter(Ecotype == "Coniferous Forest") %>%
  select(-Ecotype)
coniferous.avg1 <- coniferous.avg[grep("Reptiles", coniferous.avg$Biodiversity),]
coniferous.avg2 <- coniferous.avg[grep("Birds", coniferous.avg$Biodiversity),]
coniferous.avg3 <- coniferous.avg[grep("Mammals|plants", coniferous.avg$Biodiversity),]

mixedf.avg <- grp.avg %>%
  filter(Ecotype == "Mixed Forest") %>%
  select(-Ecotype)
mixedf.avg1 <- mixedf.avg[grep("Reptiles", mixedf.avg$Biodiversity),]
mixedf.avg2 <- mixedf.avg[grep("Birds", mixedf.avg$Biodiversity),]
mixedf.avg3 <- mixedf.avg[grep("Mammals|plants", mixedf.avg$Biodiversity),]

#' Ecotype data
# Get individual estimates and reformat table
ecofile <- paste0(derived, "/Ecotype_Estimates_tidy.csv")
eco.long <- read_tidydata(ecofile)

eco.long$Value <- na_if(eco.long$Value, "B") # where no value is provided for Counterfactual persistence
eco.long$Value <- as.numeric(eco.long$Value)

eco.wide <- eco.long %>%
  pivot_wider(names_from = Estimate, values_from = Value)

# Get averaged data
ecoresult <- (paste0(results, "/Ecotype_results.csv"))
eco.avg <- read_results(ecoresult)

#' Combine tables into a single list (and save as R objects
#' used to generate R markdown reports for expert review)
long <- list(species = sar.long, freshwater = grp.freshwater, wetland = grp.wetland,
             coniferous = grp.coniferous, mixed = grp.mixedf, ecotype = eco.long)
wide <- list(sar1 = sar.wide1, sar2 = sar.wide2,
             fresh1 = freshwater.wide1, fresh2 = freshwater.wide2, fresh3 = freshwater.wide3,
             wet1 = wetland.wide1, wet2 = wetland.wide2, wet3 = wetland.wide3,
             conifer1 = coniferous.wide1, conifer2 = coniferous.wide2, conifer3 = coniferous.wide3,
             mixed1 = mixedf.wide1, mixed2 = mixedf.wide2, mixed3 = mixedf.wide3,
             ecotype = eco.wide)
average <- list(sar1 = sar.avg1, sar2 = sar.avg2,
                fresh1 = fresh.avg1, fresh2 = fresh.avg2, fresh3 = fresh.avg3,
                wet1 = wet.avg1, wet2 = wet.avg2, wet3 = wet.avg3,
                conifer1 = coniferous.avg1, conifer2 = coniferous.avg2, conifer3 = coniferous.avg3,
                mixed1 = mixedf.avg1, mixed2 = mixedf.avg2, mixed3 = mixedf.avg3,
                ecotype = eco.avg)

saveRDS(long, file = paste0(derived, "/long.rds"))
saveRDS(wide, file = paste0(derived, "/wide.rds"))
saveRDS(average, file = paste0(derived, "/average.rds"))
