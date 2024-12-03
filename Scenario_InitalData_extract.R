#Extracting ouputs from baseline model to initialise all other scenario models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load packages -----------------------------------------------------------

library(dplyr)
library(data.table)


# Directory specifcation --------------------------------------------------
baseline_folder <- "2024-08-20_10-17_Scenario0TRUE"   # The folder that storing the default output 
path_baseline <- paste0("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/GIT_CEVA/Outputs/Calibration/", baseline_folder) # change the path to where you store baseline folder
dir.create(paste0("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/GIT_CEVA/Outputs/Initialisation/"))
dir.create(paste0("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/GIT_CEVA/Outputs/Initialisation/", baseline_folder)) # Create Initialisation folder in that directory
path_initial  <- paste0("/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/GIT_CEVA/Outputs/Initialisation/", baseline_folder) # Change to match with directory name
# Extract data ------------------------------------------------------------

burn_in  <- 7   # number of years for model to burn in (stable trends)
converge <- 500  # number of replications for model to converge
folder   <- "Calibration"

files0 <- list.files(path = path_baseline, pattern = ".csv", full.names = T)[1:converge]

lapply(files0, function(j){
 
 k <- fread(j) %>%
  filter(i == burn_in*365)
 l <- gsub(folder, "Initialisation", j)
 fwrite(x = k, file = l) 
})
