---
title: "How to run the model"
author: "Thanicha Chanchaidechachai (thanicha.c@outlook.co.th)"
date: "20/08/2024"
output: github_document
---


This simulation model consists of two parts: (I) Cow simulation model under different hormone programs; (II) GHG emissions calculation. You need to run cow simulation and save the output from cows simulation model into CSV files. These CSV outputs will be used as an input for GHG calculation model.

## Cow simulation model under different hormone programs
This R codes are link to the model from [Wicaksono et al. (2024)](https://doi.org/10.3168/jds.2023-24109). Please check this paper for the assumption of  model scenarios.

Before you run the model, please check that you already have **"ProdMod_internalFunc.R"**,  **"daysOpen_func.R"** and **"daysOpen_mat.csv"** in your project folder. These files contain the functions for weight calibration, milking day distribution, relative production and day open, which are required for the simulation model.

First, you start the model by setting parameters by running **"ProdMod_defaultParameters.R"**. This R script will create an Rdata saving parameters, which will be used later for the simulation model.

There are two parameters in **"ProdMod_defaultParameters.R"** that you need to set:

```{r, eval=F, echo=T}
xPRM$storeWD <- paste0("D:/New_cow_sensitivity/") # Directory for saving all the output. Change to your own directory
...
xPRM$CEVA_scen      <- 0      # CEVA scenarios: 0 = default, 1 = FTAI, 2 = FTAI+ED, 3 = ED+TAI
```

After setting parameters, you can start load a model. First, you start with a default model by running **"master_ProdMod_baseline.R"**. 

Finally, you can run the default model using **"master_Analysis.R"**.
<mark>Make sure that you set CEVA_scen to 0 before running.</mark>
The CSV outputs from default model will be save in a new folder creating in directory that we set in xPRM$storeWD.

After running the default model, you can run other CEVA scenarios. In this model the first seven years serving as a burn-in period to stabilise the parity distribution and the systematic hormone-based reproductive management programs are started in year 8. 

To save the time for running burn-in period again, in hormone scenario, the model will use the output from default model from the beginning of year 8 and start simulating from there. You need to run **"Scenario_InitalData_extract.R"** to extract the initial data from default model input.

Change the names of folder and paths below to match your directory.

```{r, eval=F, echo=T}
# Directory specifcation --------------------------------------------------
baseline_folder <- "2024-08-20_10-17_Scenario0TRUE"   # The folder that storing the default output 
path_baseline <- paste0("D:/New_cow_sensitivity/Outputs/Calibration/", baseline_folder) # change the path to where you store baseline folder
dir.create(paste0("D:/New_cow_sensitivity/Outputs/Initialisation/")) # Create Initialisation folder in that directory
dir.create(paste0("D:/New_cow_sensitivity/Outputs/Initialisation/", baseline_folder)) # create baseline folder ininitialisation folder
path_initial  <- paste0("/New_cow_sensitivity/Outputs/Initialisation/", baseline_folder) # Change to match with directory name
```

You only need to extract initial data once, and it can be used for all scenarios simulation.

Next, you can run the model for scenarios. Let's start with scenario 1 FTAI.
You run the **"ProdMod_defaultParameters.R"** again but change a parameter CEVA_scen to 1.


```{r, eval=F, echo=T}
...
xPRM$CEVA_scen      <- 1      # CEVA scenarios: 0 = default, 1 = FTAI, 2 = FTAI+ED, 3 = ED+TAI
```

Then you run the **"master_ProdMod_scenarios.R"** to load scenario model. In this R scipt, you change the directory to initailisation folder to match with your directory.

```{r, eval=F, echo=T}
.....
#  Read in initialisation data
initFiles <- list.files(paste0("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/GIT_CEVA/Outputs/Initialisation/", baseline_folder), full.names = T)[1:xPRM$n_sim] # Change to match your directory
initData <- lapply(initFiles, fread)
```
 
After load the model, we run the model with **"master_Analysis.R"** again. You change the baseline_folder to match with the default folder.
<mark>Make sure that you set CEVA_scen to the scenario that you want.</mark>
 
```{r, eval=F, echo=T}
.....
ptm <- proc.time()
MySim2(nFrames = xPRM$n_sim, limS = 0, clb = T, baseline_folder = "2024-08-20_10-17_Scenario0TRUE")  #the default folder
proc.time() - ptm #~ model run time
...
```
 
For each scenarios, you can repeat the steps again. From running **"ProdMod_defaultParameters.R"** , setting a new CEVA_scen and running **"master_Analysis.R"**.


## GHG emissions calculation

This R codes are link to the paper... Please check this paper for the detail of calculation.

After you get all the outputs from cow simulation model in csv files. You can use these CSV file for calculating GHG emissions. 
First, make sure that you have **"feed.composition.xlsx"** file in your folder. In this excel, you can change feed ingredients and feed composition for cows, if you have your own data.

You use the **"GHGemission.R"** for GHG emissions calculation. Before running the calculation, you set working directory to the folder that you save CSV files from cow simulation model to read the CSV files as the input for calculation.

```{r, eval=F, echo=T}
.....
# Import output from model scenario -----------
# list file names in folder
...
setwd("D:/New_cow_sensitivity/Outputs/Calibration/2024-08-20_10-17_Scenario0TRUE") # set to the directory storing CSV files
files = list.files(pattern="*.csv")
...
```

The results will save in 4 lists as follows:
<p> 1) output: save GHG emissions <br />
2) output_protein: save protein production for cows that get culled in year 18 <br />
3) output_repro: save reproduction output of cows, such as calving interval, cavling to cnception, the number of calves, the number of culled cows <br />

