# Required library -----------
library (ggplot2)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(data.table)
library(profvis)
library(directlabels)
library(esquisse)
library(Hmisc)
library(varhandle)
library(beepr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(mc2d)
library(devtools)
library(cowplot)
library(RColorBrewer)

# import feed data ------------
feed <- read_excel("feed.composition.xlsx")

# Setting parameters -------


fatP <- 4.42 # %fat from CBS, 2022 https://www.cbs.nl/nl-nl/cijfers/detail/7425eng
protP <- 3.55 # %protein 
grazing.start <- 91 # day of the year that the summer starts (grazing starts) 
grazing.end <-  274 # day of the year that the summer ends (grazing ends)
grazing.hour <- 8 # default is 8 hour of grazing per day
kg_FPCM20 <-  (0.337 + 0.116 * fatP + 0.06 * protP) * 20
kg_FPCM40  <- (0.337 + 0.116 * fatP + 0.06 * protP) * 40
n_cows <- 200

# feed related parameters
VEM.silage <- feed$Silage[feed$Value=="VEM"]
pro.silage <- feed$Silage[feed$Value=="pro.feed"]
LU.silage <- feed$Silage[feed$Value=="LU.feed"]
enteric.silage <- feed$Silage[feed$Value=="enteric.feed"]
N.silage <- feed$Silage[feed$Value=="N.feed"]
Ni.silage <- feed$Silage[feed$Value=="Ni.feed"]
Ph.silage <- feed$Silage[feed$Value=="Ph.feed"]

VEM.grass <-  feed$Pasture.grass[feed$Value=="VEM"]
pro.grass <- feed$Pasture.grass[feed$Value=="pro.feed"]
LU.grass <- feed$Pasture.grass[feed$Value=="LU.feed"]
enteric.grass <- feed$Pasture.grass[feed$Value=="enteric.feed"]
N.grass <- feed$Pasture.grass[feed$Value=="N.feed"]
Ni.grass <- feed$Pasture.grass[feed$Value=="Ni.feed"]
Ph.grass <- feed$Pasture.grass[feed$Value=="Ph.feed"]

VEM.grazecon <- feed$Grazing.concentrate[feed$Value=="VEM"]
pro.grazecon <- feed$Grazing.concentrate[feed$Value=="pro.feed"]
LU.grazecon <- feed$Grazing.concentrate[feed$Value=="LU.feed"]
enteric.grazecon <- feed$Grazing.concentrate[feed$Value=="enteric.feed"]
N.grazecon <- feed$Grazing.concentrate[feed$Value=="N.feed"]
Ni.grazecon<- feed$Grazing.concentrate[feed$Value=="Ni.feed"]
Ph.grazecon<- feed$Grazing.concentrate[feed$Value=="Ph.feed"]

VEM.stablecon <- feed$Stable.concentrate[feed$Value=="VEM"]
pro.stablecon <- feed$Stable.concentrate[feed$Value=="pro.feed"]
LU.stablecon <- feed$Stable.concentrate[feed$Value=="LU.feed"]
enteric.stablecon <- feed$Stable.concentrate[feed$Value=="enteric.feed"]
N.stablecon <- feed$Stable.concentrate[feed$Value=="N.feed"]
Ni.stablecon<- feed$Stable.concentrate[feed$Value=="Ni.feed"]
Ph.stablecon<- feed$Stable.concentrate[feed$Value=="Ph.feed"]


# Import output Sunu's model scenario -----------
# list file names in folder
setwd("D:/New_cow_simulation/Outputs/Calibration/2024-06-04_10-29_Scenario0TRUE")
#setwd("D:/New_cow_simulation/Outputs/Calibration/2024-06-04_15-46_Scenario1")
#setwd("D:/New_cow_simulation/Outputs/Calibration/2024-06-05_19-25_Scenario2")
#setwd("D:/New_cow_simulation/Outputs/Calibration/2024-06-14_08-55_Scenario3")
#setwd("D:/New_cow_sensitivity/Outputs/Calibration/Best_scene0")

#MySimCows <- read_csv("D:/New_cow_simulation/Outputs/Calibration/2024-06-04_10-29_Scenario0TRUE/MySimCows_00001.csv")

files = list.files(pattern="*.csv")
# only 1:500 files
files <-  files[1:500]

# Create list to save the output

# for GHG
output <- list()

# for protein at year 20
output_protein <- list()

# For store reproduction outcomes
output_repro <- list()
# For store culling dataframe
output_culling <- list()



#~~~~~~~~~~-----------------------------------------------------
# start for-loop for calculation -------

for (i in 1:length(files)) {

MySimCows <- data.table::fread(files[i],showProgress = FALSE)

# calculate actual yield
MySimCows$act.yield <-  MySimCows$yield - MySimCows$yield.rdc

# Calculate gestation day and month
MySimCows$gest.day <- ifelse( MySimCows$pregnant ==1 ,MySimCows$i - MySimCows$oestrus, 0)
MySimCows$gest.month <- round(MySimCows$gest.day/31,0)

#~~~~~~~~~~-----------------------------------------------------
# For summarise reproductive outcomes -------
# for milk yield
milk_yield <- MySimCows %>%  group_by (year) %>% summarise (milk_yield = sum(yield) - sum(yield.rdc))
# for total number of culling
total_cull <- MySimCows %>%  group_by (year) %>% filter(cull != 0) %>%   summarise(nCulls = n())
# for total number of fertility culling
fert_cull <- MySimCows %>%  group_by (year) %>% filter(cull == 2) %>%   summarise(fertCulls = n())
# for total number of general culling 
gen_cull <- MySimCows %>%  group_by (year) %>% filter(cull == 4) %>%   summarise(genCulls = n())
# for total number of max parity culling
maxpar_cull <- MySimCows %>%  group_by (year) %>% filter(cull == 3) %>%   summarise(maxparCulls = n())
# for total number of calves
total_calves <- MySimCows %>%  group_by (year) %>% filter(calf != 0 & par != 1 ) %>%   summarise(nCalf = n())
# calving interval median and 90%quantile
calving_interval <-  MySimCows %>%  filter(calf == 1, prevdaycalf != 0) %>% mutate(CI = daycalf - prevdaycalf) %>% group_by(year) %>% summarise(mean_CI = mean(CI), lower_CI= quantile(CI, probs = .05),upper_CI= quantile(CI, probs = .95) ) 
# calving to conception median and 90%quantile
calving_conception <- MySimCows %>% filter(detoest == 1, n_insem == 1) %>%  mutate(CC = dim) %>% group_by(year) %>% summarise(mean_CC = mean(CC), lower_CC= quantile(CC, probs = .05),upper_CC= quantile(CC, probs = .95) ) 

repro <- milk_yield %>%  left_join(total_cull, by='year') %>%
  left_join(fert_cull, by='year') %>%
  left_join(gen_cull, by='year') %>%
  left_join(maxpar_cull, by='year') %>%
  left_join(total_calves, by='year') %>%
  left_join(calving_interval , by='year') %>%
  left_join(calving_conception , by='year') %>% mutate(iter= i)

output_repro[[i]] <-  repro

## To get age, weight at culling and reason for culling
output_culling[[i]] <- MySimCows %>%  filter(cull != 0)


rm(repro, milk_yield,total_cull,total_calves,calving_interval,calving_conception)
#~~~~~~~~~~-----------------------------------------------------

# For calculation of GHG of individual cow ---------

Output.GHG.cowday <- mutate (MySimCows,
# Calculate FPCM per cow per day
  kg_fat = (act.yield * fatP/ 100), # calculate fat content in milk
  kg_prot = (act.yield * protP/ 100), # calculate protein content in milk
  kg_FPCM = (0.337 + 0.116 * fatP + 0.06 * protP) * act.yield, # calculate Fat-and-protein-corrected milk (FPCM) from milk yield

# Calculate VEM for overall milking  + maintain # (Handbook BEX, 2023 P.11)
# if the milk yield >40, the cow only get the VEM from concentrate feed up to 40 kg
  VEM.maintain = ifelse(act.yield > 0  ,  42.4 * (weight^0.75) * (1 + (kg_FPCM - 15) * 0.00165),42.4 * (weight^0.75) * (1 + (0 - 15) * 0.00165) ),
  VEM.milk = ifelse(act.yield > 0, 442 * kg_FPCM * (1 + (kg_FPCM -15) * 0.00165) ,0),
# VEM for 
# Additional VEM for summer *only for lactating cows and heifer *Dry cow not grazing
# Assume that the grazing period 183 days (1st May (121) - 31 October (304)) in summer month 7 hour per day (CBS 2021) 
# VEM for grazing = 419 (Table2 Handbook BEX, 2023)
# Only lactating cow grazing
  VEM.grazing = ifelse(act.yield > 0 & day %in% c(grazing.start:grazing.end) , 419, 0),  
# Additional VEM for growth 
# Lactating heifers and 2nd parity cows still have to grow: parity 1 = 625, parity 2 = 325, parity 3 = 125, parity 4 = 125  (Table 1.1 CVB, 2022)
  VEM.growth = case_when(par == 1 ~ 625, par == 2 ~ 325, par ==3 ~ 125, par ==3 ~ 125 ,.default = 0),
# Additional VEM for pregnant for dairy cows from the 4th month of gestation. (Table 1.3 CVB, 2022)
# 4 month  = 250, 5 month = 400, 6 month = 650, 7 month = 1100, 8 month = 1700, 9 month = 2750
  VEM.pregnant = case_when(gest.month  == 4 ~ 250, gest.month  == 5 ~ 400,  gest.month  == 6 ~ 650,gest.month  == 7 ~ 1100, gest.month  == 8 ~ 1700, gest.month  == 9 ~ 2750,.default = 0),
  
# total VEM
  VEM.total = VEM.maintain+ VEM.milk + VEM.grazing + VEM.growth+ VEM.pregnant,

# Calculate VEM for maintain + milk until 20 kg
  VEM.maintain20 = VEM.maintain +  (442 * kg_FPCM20 * (1 + (kg_FPCM20 -15) * 0.00165)) ,
# Calculate VEM for maintain + milk until 40 kg
 VEM.maintain40 = (442 * kg_FPCM40 * (1 + (kg_FPCM40 -15) * 0.00165))+ VEM.maintain  + VEM.grazing + VEM.growth+ VEM.pregnant,

# VEM from concentrate and roughage # roughage will support maintain and milk yield up to 20 kg
# milk more 20 kg + pregnant growth graze are from concentrate
  VEM.roughage = ifelse (VEM.maintain20 > VEM.total, VEM.total, VEM.maintain20 ),
  VEM.concentrate = ifelse (VEM.maintain20 > VEM.total, 0, VEM.total - VEM.maintain20 ),
# to limit the max VEM to 40 kg milk
  VEM.concentrate = ifelse (VEM.total> VEM.maintain40, VEM.maintain40-VEM.maintain20, VEM.concentrate),
# For roughage, we assume that the cow graze for 8 hours, which approximately (2+0.75*6) = 6.5 kg DM of fresh grass (Table 2, Handbook BEX 2023)
# only lactating cow can graze, the dry cow will not graze
  grass.intake =  ifelse (act.yield > 0 & day %in% c(grazing.start:grazing.end),6.5, 0), # kg DM
# we deduct that from roughage VEM and the rest is come from 50:50 grass 
  silage.intake = ifelse (act.yield > 0 & day %in% c(grazing.start:grazing.end), (VEM.roughage - (6.5*VEM.grass)) /VEM.silage,
                            VEM.roughage / VEM.silage),# kg DM


# for concentrate feed we assume for non-grazing =  concentrate:high protein concentrate: wet concentrate = 0.5:0.35:0.15 for non grazing (CBS, 2023)
# for grazing = concentrate: wet concentrate = 0.85: 0.15
# the DM, VEM of concentrate were calculate separately in excel file feed.composition
  grazecon.intake = ifelse (act.yield > 0 & day %in% c(grazing.start:grazing.end), VEM.concentrate /VEM.grazecon,0), # kg DM
  stablecon.intake = ifelse (grazecon.intake == 0,  VEM.concentrate/VEM.stablecon, 0),# kg DM

  total.DMI = grazecon.intake + stablecon.intake + grass.intake + silage.intake,
# Manure excretion calculation -----------
# calculate manure excretion from Nennich et al, 2005 from DMI 
# we separate manure excretion from stable and pasture, we assume that the cow graze for 8 hour (1/3 day), and dry cow did
  ME.pasture = ifelse (act.yield > 0 & day %in% c(grazing.start:grazing.end), ((total.DMI * 2.63)  + 9.4)*1/3, 0),
  ME.stable = ifelse (act.yield > 0 & day %in% c(grazing.start:grazing.end), ((total.DMI * 2.63)  + 9.4)*2/3, ((total.DMI * 2.63)  + 9.4)),
#  Nitrogen calculation ----------
# To calculate GHG from manure management, firstly we need to calculate the kg nitrogen in manure, kg TAN
# Calculate kg nitrogen in manure
# N in feed is calculated from from %crude protein in feed/ 6.25 (IPCC 2006, p. 58)

# Nitrogen in feed (kg N / kg DM) is calculated from excel sheet
  N.feed = (grass.intake * N.grass)  + (silage.intake * N.silage ) + (grazecon.intake * N.grazecon) + (stablecon.intake * N.stablecon)  ,  # kg N per cow per day,

# Calculate TAN, N excretion = N intake (absorbed protein) - N retention in milk, growth and pregnant (Velthof 2010)
# N intake calculated from DM * %CP * digestibility/ 6.25 * we will calculate this in excel file
  N.intake = (grass.intake * Ni.grass) +(silage.intake * Ni.silage ) +  (grazecon.intake * Ni.grazecon) + (stablecon.intake * Ni.stablecon) , # Kg N  per cow per day,

# Nitrogen retention in milk (kg N per cow per day)  (check Hand book for nitrogen)
  N.fix.milk =  act.yield  * (protP/100) /6.38,  # 6.38 is conversion factor from milk protein to milk N (IPCC 2006, equation 10.33), 
# Nitrogen retention for growth 
# N content from heifer = 23.1 g /kg (assuming 540 kg) , N content in fully mature dairy cow = 22.5 g/kg (assuming 650 kg)
# On average the N fixation for growth from heifer to parity 3  = ((650*22.5) - (23.1*540)) /1000 = 2.151 kg from heifer to cow parity 3, 3 year. Average N per day = 2.151/(365*3) = 0.00196 N kg /day-1
# Another calculation using IPCC, 2006 Equation 10.33 (0.134 * (268-(7.03*4.3125/0.134))) /(1000*6.25) = 0.00089522 N kg per cow per day
  N.fix.growth = ifelse (par <= 3, 0.00196, 0),
# Nitrogen retention for pregnant
# from Bex 2023 Table 6 N content in calf = 29.4 g/kg assume that 1 calf weight  = 44 kg, 29.4*44/1000 = 1.2936 N kg per calf
# the gestation period = 283 day = 1.2936 / 283 N kg per day
  N.fix.gest = ifelse (pregnant ==1, 29.4*44/1000/283, 0),

#  N in manure = N in feed  - N retention
  N.manure = N.feed - N.fix.milk - N.fix.growth - N.fix.gest,
# N in urine = TAN = N.intake (absorbed protein) - N retention
  N.TAN = N.intake - N.fix.milk - N.fix.growth - N.fix.gest,

#' GHG from manure management will be calculate in period of 1 year and separately between manure in pasture and stable
#' To do that we need to calculate the N.manure in stable, N.manure.pasture and N.TAN in stable  (we don't need N.TAN pasture for calculation)
#' We assume the amount of manure is proportional to grazing hour and grazing period
#' Only lactating cow grazing in summer, dry cow will not graze during summer

# N.manure in stable 
  N.TAN.pasture = ifelse(act.yield > 0 & day %in% c(grazing.start:grazing.end), N.TAN * grazing.hour/24,  0),
# N manure in pasture
  N.manure.pasture = ifelse(act.yield > 0 & day %in% c(grazing.start:grazing.end), N.manure * grazing.hour/24,  0),
# N.TAN stable 
  N.TAN.stable = ifelse(act.yield > 0 & day %in% c(grazing.start:grazing.end),  N.TAN * (1- (grazing.hour/24)), N.TAN),


# Phosphate calculation---------
Ph.feed = (grass.intake * Ph.grass)  + (silage.intake * Ph.silage ) + (grazecon.intake * Ph.grazecon) + (stablecon.intake * Ph.stablecon)  ,  # kg P per cow per day,
Ph.fix.milk = 0.97/1000 * act.yield, # 0.97 g / kg milk (Bex, 2023 Table 6) 

#P content from heifer = 7.4 g /kg (assuming 540 kg) , P content in fully mature dairy cow = 7.4 g/kg (assuming 650 kg)
# On average the P fixation for growth from heifer to parity 3  = ((650*7.4) - (7.4*540)) /1000 = 0.814 kg 
# from heifer to cow parity 3, 3 year. Average P per day = 0.8141/(365*3) = 0.00074 P kg /day-1

Ph.fix.growth = ifelse (par <= 3, 0.00074, 0), 

# P for gestation: Calf phosphorus(P) content (g/kg) = 8.0 if calf wight 44 kg = 352 g 
# divided by 283 gestation period = 357/283 = 1.24 g per day = 0.00124 kg per day
Ph.fix.gest =  ifelse (pregnant ==1, 0.00124, 0),

Ph.manure = Ph.feed - Ph.fix.milk - Ph.fix.growth  - Ph.fix.gest,

# (1) GHG Feed production --------
# 1.1 gCO2 emission for production of ingredients 
  # This value will be from excel file. We take this value from feed print.
  GHG.feed = ((grass.intake * pro.grass) + (silage.intake * pro.silage )  + (grazecon.intake * pro.grazecon) + (stablecon.intake * pro.stablecon)), # GHG from feed production (kg CO2e)

# 1.2 gCO2 emission for LULuc
# This value will be from excel file. We take this value from feed print.
  GHG.LU = ((grass.intake * LU.grass) + (silage.intake * LU.silage )  +  (grazecon.intake * LU.grazecon) + (stablecon.intake * LU.stablecon)),# GHG from Land use land use change (kg CO2e)

# (2) GHG Enteric methane  ----------
# # This value will be from excel file. We take this value from feed print.
# GHG enteric fermentation (kg CH4)
# *27 is to convert methane to CO2 equivalent, and 273 to N2O (IPCC AR6, 2022 Table 7.15)
  GHG.enteric = ((grass.intake * enteric.grass) + (silage.intake * enteric.silage )  +  (grazecon.intake * enteric.grazecon) + (stablecon.intake * enteric.stablecon)) * 27,

# (3) GHG System substitution --------
  # GHG reduced from meat substitution from calves and culled cows
#'Emissions of GHGs related to the production of meat from chicken (5.6 kg CO2e/kg edible product), pork (7.3 kg CO2e/kg edible product) 
#'or beef (23.4 kg CO2e/kg edible product) (Van Middelaar et al., 2014a) were weighted to 
#'an average emission factor (10 kg CO2e/kg edible product) based on the average consumption of chicken (28 kg), pork (22 kg), 
#'and beef (14 kg) per capita in OECD countries (OECD, 2015). For culled cows (540e650 kg live weight) and calves (44 kg live weight), 
#'it was assumed that 1 kg of animal live weight consisted of 0.406 kg edible product (Van Middelaar et al., 2014a).
#'* 10 kg CO2e/ kg edible product ) can change if we new data on CO2 emission per edible animal product and average consumption per capita
#'# Check new calculation in onenote

  GHG.substitute.calves = ifelse(calf == "1", 44* 0.406 * -10.4, 0 ), # assume 44 kg calf, we assume all calves were sell form meat
  GHG.substitute.cull = ifelse(cull != "0" , 5601 - (weight * 0.406 * 10.4) , 0 ), 

  #(4) GHG Manure management  ----------
# Calculate the emission from stable manure
# direct N2O-N  from stable  (De vries 2011, Table 3.2)
# 44/28 to convert from N2O-N to N2O (IPCC 2006, equation 10.25)
# 1 N2O = 273 CO2 (IPCC AR6, 2022 Table 7.15, page 1034)
GHG.dN2O.stable = N.TAN.stable * 0.0015 * (44/28) * 273, 

# indirect N2O-N from stable 
# Indirect N2O come from NH3-N, NOx-N, which calculated from TAN
# check De vries 2011, Table 3.2 how to calculate NH3-N, NOx-N from TAN and check IPCC 2006 Equation 10.27 for indirect N2O emission factor
GHG.iN2O.stable = ( N.TAN.stable * 0.1 * 0.01 * (44/28)) + ( N.TAN.stable * 0.0015 * 0.01 * (44/28))* 273,

# direct N2O-N  from pasture (Vonk et al. 2010 page 70)
GHG.dN2O.pasture =  N.manure.pasture * 0.033 * (44/28) * 273,

# Indirect N2O-N from pasture
# Indirect N2O come from NH3-N which calculated from TAN and  NOx-N, NO3-N which calculated from N
# NH3-N calculation check Vonk et al. 2010 P. 62 --> 1.98 * 10^-5 * (N-content ration)^3.664; N-content ration is g N/kg DM feed  
# NOx-N calculation check Vonk et al2010 P.65
# NO3-N calculation check Velthof, 2011 Fracleach P.10
# Emission factor from NH3-N, NOx-N to N2O-N = 0.01 (IPCC 2006 Equation 10.27)
# Emission factor from NO3N = 0.0075 (IPCC, 2006  Equation 10.29)
GHG.iN2O.pasture = ((( N.TAN.pasture * 0.053)*0.01) + ( N.manure.pasture * 0.012 *0.01) + 
                      (N.manure.pasture * 0.12 * 0.0075)) * (44/28) *273,

# Emission from CH4 
# Manure amount (ton) * OM * B0 * MCF * 0.67 * 27   (Vellinga, 2013 ) (0.67 is convert m3 to kg check IPCC, 2006) (27 to convert CH4 to CO2)
# OM, MCF and B0 are from the (Van der Maas , 2011 Table A8.4, A8.6) 
# for stable OM = 64 per 1000 kg manure, B0F = 0.25,MCF = 0.17
# for pasture OM = 64 per 1000 kg manure, B0 = 0.25, MCF = 0.01
# Manure production  check https://longreads.cbs.nl/dierlijke-mest-en-mineralen-2021/graasdieren/ 
# 26000 manure kg/cow/year in stable and 3000 kg/cow/ year in pasture in total 29000 kg manure per cow per year
# we assume that 2/3 of herd graze, 1/3 is dry cow not grazing
# We changed calculation method to using manure excretion calculated from DMI
# CH4 Emission from  stable 
GHG.CH4.stable = (ME.stable/1000) * 64 * 0.25 * 0.17 * 0.67 * 27 ,
# CH4 emission from pasture
GHG.CH4.pasture =  (ME.pasture/1000) * 64 * 0.25 * 0.01 * 0.67 * 27,

# sum GHG from manure
GHG.manure =  GHG.dN2O.stable+ GHG.iN2O.stable + GHG.dN2O.pasture + GHG.iN2O.pasture + GHG.CH4.stable +GHG.CH4.pasture,

# (5) Calculate protein -----
# Note that protein here is in unit of kg protein 
# protein from milk
prot.milk = act.yield * 3.55/100,
# protein from meat from culled cows
# Average %protein from raw beef = 21%, 0.406 kg edible product per kg BW (Van Middelaar et al., 2014a)
prot.cull = ifelse(cull != "0" ,weight * 0.406 * 0.21 , 0 ),
# protein from calf, assume 44 kg BW of calves and  0.406 kg edible product per kg BW (Van Middelaar et al., 2014a)
prot.calf = ifelse(calf == "1", 44 * 0.406 * 0.21 , 0 ))


#~~~~~~~~~~-----------------------------------------------------
# Sum by cow ID for protein in year 18---------

#  Taking only culled cow in year 18 to calculate protein ------
# select cow UID that get cull on year 18
ID_cully18 <- MySimCows$UID[MySimCows$year == 18 & MySimCows$cull != 0 ] 
Output.GHG.cowday_year18 <- Output.GHG.cowday[Output.GHG.cowday$UID %in% ID_cully18,]
# check if all cows start at parity 1 & DIM 1, nrow should be equal to ID_cully20
# nrow(MySimCows[MySimCows$par ==1 & MySimCows$dim ==1,]) == length(ID_cully20)


Output.GHG.protein <- Output.GHG.cowday_year18 %>% group_by(UID) %>% 
  summarise ( sum.kgFPCM = sum (kg_FPCM),
              sum.milk = sum(act.yield),
              sum.GHG.feed = sum(GHG.feed),
              sum.GHG.LU = sum(GHG.LU),
              sum.GHG.feedLU = sum.GHG.feed  + sum.GHG.LU,
              sum.GHG.enteric = sum (GHG.enteric), # 1 CH4 = 27 CO2e
              sum.GHG.substitute.calves = sum(GHG.substitute.calves),
              sum.GHG.substitute.cull = sum (GHG.substitute.cull),
              sum.GHG.manure = sum(GHG.manure),
              sum.N.manure = sum(N.manure),
              sum.Ph.manure = sum(Ph.manure),
              sum.prot.milk = sum (prot.milk),
              sum.prot.cull = sum(prot.cull),
              sum.prot.calf = sum(prot.calf),
              sum.GHG =  sum.GHG.feed + sum.GHG.LU + sum.GHG.enteric +sum.GHG.manure,
              sum.GHG.system =  sum.GHG.feed + sum.GHG.LU + sum.GHG.enteric + sum.GHG.manure + sum.GHG.substitute.cull + sum.GHG.substitute.calves,
              sum.prot =sum.prot.milk + sum.prot.cull +sum.prot.calf,
              GHH.protein = sum.GHG/sum.prot
              
  )%>%
  as.data.frame()

# For protein
output_protein[[i]] <-  Output.GHG.protein

rm(Output.GHG.cowday_year18,Output.GHG.protein)
#~~~~~~~~~~-----------------------------------------------------

# Sum by cow year for GHG ---------
Output.GHG.farmyear <- Output.GHG.cowday %>% group_by(year) %>% 
  summarise ( sum.kgFPCM = sum (kg_FPCM),
              sum.milk = sum(act.yield),
              # for GHG manure calculation
              sum.N.TAN.pasture = sum(N.TAN.pasture),
              sum.N.manure.pasture = sum(N.manure.pasture),
              sum.N.TAN.stable = sum(N.TAN.stable),
              sum.ME.pasture =sum(ME.pasture),
              sum.ME.stable = sum(ME.stable),
              # for GHG feed and enteric calculation
              grazecon.intake =  sum (grazecon.intake),
              stablecon.intake = sum (stablecon.intake), 
              grass.intake = sum (grass.intake ),
              silage.intake = sum(silage.intake),
              # GHG calculation
              sum.GHG.feed = sum(GHG.feed),
              sum.GHG.LU = sum(GHG.LU),
              sum.GHG.feedLU = sum.GHG.feed  + sum.GHG.LU,
              sum.GHG.enteric = sum (GHG.enteric), # 1 CH4 = 27 CO2e
              sum.GHG.substitute.calves = sum(GHG.substitute.calves),
              sum.GHG.substitute.cull = sum (GHG.substitute.cull),
              sum.GHG.manure = sum(GHG.manure),
              sum.N.manure = sum(N.manure),
              sum.Ph.manure = sum(Ph.manure),
              sum.prot.milk = sum (prot.milk),
              sum.prot.cull = sum(prot.cull),
              sum.prot.calf = sum(prot.calf),
              sum.GHG =  sum.GHG.feed + sum.GHG.LU + sum.GHG.enteric + sum.GHG.manure,
              sum.GHG.system =  sum.GHG.feed + sum.GHG.LU + sum.GHG.enteric + sum.GHG.manure + sum.GHG.substitute.cull  + sum.GHG.substitute.calves  ,
              sum.prot =sum.prot.milk + sum.prot.cull +sum.prot.calf,
              GHH.protein = sum.GHG/sum.prot
              
  )%>%
  as.data.frame()

#For total GHG
output[[i]] <-  Output.GHG.farmyear
#~~~~~~~~~~-----------------------------------------------------
# Remove an for-loop -----
rm(Output.GHG.cowday, MySimCows, Output.GHG.farmyear)
# the counter for iterations
cat(paste(i, " ")); flush.console()
}
#~~~~~~~~~~-----------------------------------------------------

# combine list to dataframe ------
gc()
setwd("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/R_Ceva GHG")


# for baseline
baseline_output <- bind_rows(output, .id = "iter")
baseline_output_protein <- bind_rows(output_protein, .id = "iter")
baseline_output_repro <- bind_rows(output_repro, .id = "iter")
baseline_output_culling  <- bind_rows(output_culling , .id = "iter")

saveRDS(baseline_output, "baseline_output.rds")
saveRDS(baseline_output_protein, "baseline_output_protein.rds")
saveRDS(baseline_output_repro, "baseline_output_repro.rds")
saveRDS(baseline_output_culling, "baseline_output_culling.rds")

# for Scenario_1 
scene1_output <- bind_rows(output, .id = "iter")
scene1_output_protein <- bind_rows(output_protein, .id = "iter")
scene1_output_repro <- bind_rows(output_repro, .id = "iter")
scene1_output_culling  <- bind_rows(output_culling , .id = "iter")

saveRDS(scene1_output, "scene1_output.rds")
saveRDS(scene1_output_protein, "scene1_output_protein.rds")
saveRDS(scene1_output_repro, "scene1_output_repro.rds")
saveRDS(scene1_output_culling, "scene1_output_culling.rds")


# for Scenario_2 
scene2_output <- bind_rows(output, .id = "iter")
scene2_output_protein <- bind_rows(output_protein, .id = "iter")
scene2_output_repro <- bind_rows(output_repro, .id = "iter")
scene2_output_culling  <- bind_rows(output_culling , .id = "iter")

saveRDS(scene2_output, "scene2_output.rds")
saveRDS(scene2_output_protein, "scene2_output_protein.rds")
saveRDS(scene2_output_repro, "scene2_output_repro_DoubOV.rds")
saveRDS(scene2_output_culling, "scene2_output_culling.rds")



# for Scenario_3 
scene3_output <- bind_rows(output, .id = "iter")
scene3_output_protein <- bind_rows(output_protein, .id = "iter")
scene3_output_repro <- bind_rows(output_repro, .id = "iter")
scene3_output_culling  <- bind_rows(output_culling , .id = "iter")

saveRDS(scene3_output, "scene3_output.rds")
saveRDS(scene3_output_protein, "scene3_output_protein.rds")
saveRDS(scene3_output_repro, "scene3_output_repro.rds")
saveRDS(scene3_output_culling, "scene3_output_culling.rds")

#~~~~~~~~~~-----------------------------------------------------

# Plot GHG with tFPCM  ------
setwd("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/R_Ceva GHG")

# GHG_default <- bind_rows( baseline_output,  .id = "scenario")
GHG_default <- bind_rows( baseline_output, scene1_output,scene2_output,scene3_output, baseline_output_best,  .id = "scenario")

#Save in GHG name
# default one 
GHG <- GHG_default

# combine feed production and LU
GHG$sum.GHG.feedLU <- GHG$sum.GHG.feed+GHG$sum.GHG.LU
GHG$sum.GHG <-  GHG$sum.GHG.feed + GHG$sum.GHG.LU + GHG$sum.GHG.enteric + GHG$sum.GHG.manure
GHG$sum.GHG.system <-  GHG$sum.GHG.feed + GHG$sum.GHG.LU + GHG$sum.GHG.enteric + GHG$sum.GHG.manure + GHG$sum.GHG.substitute.calves + GHG$sum.GHG.substitute.cull
# Calculate all GHG per tFPCM
GHG <- GHG %>% mutate_at(vars(sum.GHG.feed:sum.GHG.system),list(tFPCM=~./(sum.kgFPCM/1000)))

# Calculate median and quantile of all value
GHG_sum <-  gather(GHG[GHG$year!= "7",], value, emission, sum.kgFPCM:sum.GHG.system_tFPCM, factor_key=TRUE)
GHG_sum <- GHG_sum[,-2] # delete iter
GHG_sum$scenario <- as.factor(GHG_sum$scenario)


GHG_sum_long <- GHG_sum %>% group_by(scenario, year, value) %>%   
  dplyr::summarize(mean = mean(emission), 
            sd = sd(emission), 
            q5 = quantile(emission, probs = .05),
            q50 = quantile(emission, probs = .5),
            q95 = quantile(emission, probs = .95))
# export in excel to make table
# write_csv(GHG_sum_long, "table_output2.csv")
# write_csv(Output.GHG.cowday, "Output.GHG.cowday.csv")

#label
GHG_sum_long$scenario <- factor(GHG_sum_long$scenario, levels = c(1, 2, 3, 4,5),
                                labels = c("Default", "FTAI", "FTAI+HD", "HD","best"))


# plot stacked bar for each year
ggplot( GHG_sum_long[ GHG_sum_long$value %in% c( "sum.GHG.feedLU_tFPCM","sum.GHG.enteric_tFPCM", "sum.GHG.manure_tFPCM","sum.GHG.substitute_tFPCM", "sum.GHG.heifer.replacement_tFPCM"),], 
        aes(fill=value, y=mean, x=scenario)) + geom_bar(position="stack", stat="identity") +
  labs(title = "GHG emission for each scenario",
       x = "Scenario", y = "GHG emission (kg CO2e/ tFPCM)", color = "Emission") + 
  scale_fill_viridis_d(labels = c( "enteric fermentation", "meat substitution","heifer replacement", "manure management", "feed production"))+facet_grid(. ~ year)


# plot line graph for each value for each year to see the trend change

# for milk production
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.kgFPCM" & GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point() + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Milk production",
       x = "year", y = "milk production(kg FPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for overall without system expansion
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production without system expansion",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for overall with system expansion
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.system_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production with system expansion",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()



# for feed production
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.feedLU_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production for feed production",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for Manure
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.manure_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production from manure management",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()

# for enteric fermentation
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.enteric_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production from enteric fermentation",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for Overall with system expansion 
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.system_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production with system expansion",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()



# for calves
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.substitute.calves_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Reduced GHG emissions relative to milk production from calves",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()

# for substitute culling
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.substitute.cull_tFPCM" & GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production for culled cows",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# Plot GHG without tFPCM ---------

# for overall with system expansion
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.system_tFPCM"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions relative to milk production with system expansion",
       x = "year", y = "GHG emission (kg CO2e/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()

# plot line graph for each value for each year to see the trend change


# for overall without system expansion
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions without system expansion",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for overall with system expansion
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.system"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions  with system expansion",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()



# for feed production
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.feedLU"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions for feed production",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for Manure
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.manure"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions from manure management",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()

# for enteric fermentation
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.enteric"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions from enteric fermentation",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()



# for calves
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.substitute.calves"& GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Reduced GHG emissions from calves",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()

# for substitute culling
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.GHG.substitute.cull" & GHG_sum_long$year <=18,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "GHG emissions for culled cows",
       x = "year", y = "GHG emission (kg CO2e)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) + theme_bw()


# Boxplot for total GHG -----
#label
GHG$scenario <- factor(GHG$scenario, levels = c(1, 2, 3, 4),
                                labels = c("Default", "FTAI", "FTAI+ED", "ED+TAI"))

GHG$sum.GHG.system_tFPCM <- GHG$sum.GHG.system/GHG$sum.kgFPCM*1000
#choose color

display.brewer.all(colorblindFriendly = TRUE)

ggplot(GHG[ GHG$year ==18 & GHG$scenario%in% c(1,2,3,4),], aes(x=scenario, y=sum.GHG.system, fill=scenario)) +
  geom_boxplot(show.legend = FALSE)+theme_bw()+ scale_fill_brewer(palette="Set2") + 
  labs(x = "Scenario", y = "GHG emission (kg CO2e)",  color = "Scenario")



ggplot(GHG[ GHG$year ==18,], aes(x=scenario, y=sum.GHG.system_tFPCM, fill=scenario)) +
  geom_boxplot()+theme_bw()+ scale_fill_brewer(palette="Set2") + 
  labs(x = "Scenario", y = "GHG emission (kg CO2e/ tFPCM)",  color = "Scenario")




ggplot(GHG[ GHG$year ==18,], aes(x=scenario, y=sum.GHG.feedLU_tFPCM, fill=scenario)) +
  geom_boxplot()+theme_bw()+ scale_fill_brewer(palette="Set2") + 
  labs(x = "Scenario", y = "GHG emission (kg CO2e/ tFPCM)",  color = "Scenario")


#~~~~~~~~~~~~~~~~~~~~~~~----------------------------------------------------
# Plot GHG per protein in year 18------
setwd("C:/Users/Chanc004/OneDrive - Wageningen University & Research/Side projects/Ceva_model environment/R_Ceva GHG/Results")

# GHG_prot <- bind_rows( baseline_output_protein2)

GHG_prot <- bind_rows( baseline_output_protein, scene1_output_protein,scene2_output_protein,scene3_output_protein,  .id = "scenario")

GHG <- GHG_prot

# there might be some cow that getting culled without producing anything yet by chance
# remove that kind of cows
GHG <- GHG[GHG$sum.kgFPCM > 0,]

# the data GHG protein is from cows culled in year 18
GHG$lifetime_GHG <- GHG$sum.GHG.feed + GHG$sum.GHG.LU + GHG$sum.GHG.enteric + GHG$sum.GHG.manure  + 5601 # 5601 is heifer rearing
GHG$GHG_per_protein <- GHG$lifetime_GHG/GHG$sum.prot
sum(GHG$lifetime_GHG[GHG$scenario == 1] )/length(GHG$lifetime_GHG[GHG$scenario == 1])
sum(GHG$lifetime_GHG[GHG$scenario == 2] )/length(GHG$lifetime_GHG[GHG$scenario == 2])
sum(GHG$lifetime_GHG[GHG$scenario == 3] )/length(GHG$lifetime_GHG[GHG$scenario == 3])
sum(GHG$lifetime_GHG[GHG$scenario == 4] )/length(GHG$lifetime_GHG[GHG$scenario == 4])

sum(GHG$sum.prot[GHG$scenario == 1] )/length(GHG$sum.prot[GHG$scenario == 1])
sum(GHG$sum.prot[GHG$scenario == 2] )/length(GHG$sum.prot[GHG$scenario == 2])
sum(GHG$sum.prot[GHG$scenario == 3] )/length(GHG$sum.prot[GHG$scenario == 3])
sum(GHG$sum.prot[GHG$scenario == 4] )/length(GHG$sum.prot[GHG$scenario == 4])

sum(GHG$GHG_per_protein[GHG$scenario == 1] )/length(GHG$GHG_per_protein[GHG$scenario == 1])
sum(GHG$GHG_per_protein[GHG$scenario == 2] )/length(GHG$GHG_per_protein[GHG$scenario == 2])
sum(GHG$GHG_per_protein[GHG$scenario == 3] )/length(GHG$GHG_per_protein[GHG$scenario == 3])
sum(GHG$GHG_per_protein[GHG$scenario == 4] )/length(GHG$GHG_per_protein[GHG$scenario == 4])

GHG %>% group_by(scenario) %>% dplyr::summarize(life_time_GHG =mean(lifetime_GHG ), 
                                                total_protein = mean(sum.prot)  ,
                                                GHG_per_protein = mean(GHG_per_protein) )
GHG %>% dplyr::summarise(GHG_per_protein = mean(GHG_per_protein))
#~~~~~~~~~~~~~~~~~~~~~~~----------------------------------------------------
# Plot nitrogen and phosphorus -----
GHG_default <- bind_rows( baseline_output, scene1_output,scene2_output,scene3_output,  .id = "scenario")

#Save in GHG name
# default one 
GHG <- GHG_default
# combine feed production and LU
GHG$sum.GHG.feedLU <- GHG$sum.GHG.feed+GHG$sum.GHG.LU
GHG$sum.GHG <-  GHG$sum.GHG.feed + GHG$sum.GHG.LU + GHG$sum.GHG.enteric + GHG$sum.GHG.manure
GHG$sum.GHG.system <-  GHG$sum.GHG.feed + GHG$sum.GHG.LU + GHG$sum.GHG.enteric + GHG$sum.GHG.manure - GHG$sum.GHG.substitute.calves + GHG$sum.GHG.substitute.cull
# Calculate all GHG per tFPCM
GHG <- GHG %>% mutate_at(vars(sum.GHG.feed:sum.GHG.system),list(tFPCM=~./(sum.kgFPCM/1000)))

# Calculate median and quantile of all value
GHG_sum <-  gather(GHG[GHG$year!= "7",], value, emission, sum.kgFPCM:sum.GHG.system_tFPCM, factor_key=TRUE)
GHG_sum <- GHG_sum[,-2] # delete iter
GHG_sum$scenario <- as.factor(GHG_sum$scenario)


GHG_sum_long <- GHG_sum %>% group_by(scenario, year, value) %>%   
  dplyr::summarize(mean = mean(emission), 
                   sd = sd(emission), 
                   q5 = quantile(emission, probs = .05),
                   q50 = quantile(emission, probs = .5),
                   q95 = quantile(emission, probs = .95))
# export in excel to make table
# write_csv(GHG_sum_long, "table_output2.csv")


#label
GHG_sum_long$scenario <- factor(GHG_sum_long$scenario, levels = c(1, 2, 3, 4),
                                labels = c("Default", "FTAI", "FTAI+HD", "HD"))




# for nitrogen related to milk production

ggplot(GHG_sum_long[GHG_sum_long$value =="sum.N.manure_tFPCM"& GHG_sum_long$year <=20,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Nitrogen excretion relative to milk production",
       x = "year", y = "Nitrogen excretion (kg N/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for nitrogen 
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.N.manure"& GHG_sum_long$year <=20,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Nitrogen excretion",
       x = "year", y = "Nitrogen excretion (kg N)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for nitrogen related to milk production

ggplot(GHG_sum_long[GHG_sum_long$value =="sum.N.manure_tFPCM"& GHG_sum_long$year <=20,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Nitrogen excretion relative to milk production",
       x = "year", y = "Nitrogen excretion (kg N/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()


# for Phosphorus
ggplot(GHG_sum_long[GHG_sum_long$value =="sum.Ph.manure"& GHG_sum_long$year <=20,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Phosphorus excretion",
       x = "year", y = "Phosphorus excretion (kg N)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()

# for phosphorus related to milk production

ggplot(GHG_sum_long[GHG_sum_long$value =="sum.Ph.manure_tFPCM"& GHG_sum_long$year <=20,], aes(year, q50, color = scenario)) +
  geom_line() +
  geom_point()  + scale_x_continuous(breaks = c(8,10,12,14,16,18,20)) + 
  labs(title = "Phosphorus excretion relative to milk production",
       x = "year", y = "Phosphorus excretion (kg P/ tFPCM)",
       color = "scenario")+ 
  geom_ribbon(aes(ymin =q5, ymax= q95), linetype=2, alpha=0.1) +theme_bw()



##~~~~~~~~~~~~~~~~~~-------------------------------------------------------------

# Plot graph reproduction --------

# Join reproduction -------------------------
# repro <-  bind_rows( baseline_output_repro2)
repro <-  bind_rows( baseline_output_repro, scene1_output_repro , scene2_output_repro, scene3_output_repro, baseline_output_repro_best,   .id = "scenario")

#saveRDS(repro, "repro.rds")

summary (repro)
repro$scenario <- as.factor(repro$scenario)


#Function percentile ----
Upper <- function(x) {quantile(x, probs = .95) }
Lower <- function(x) {quantile(x, probs = .05) }
Median <- function(x) {quantile(x, probs = .5) }

# Summarise by year and scenario -----
repro_my <- repro %>% group_by (year, scenario) %>% summarise(across(c("milk_yield", "nCulls", "nCalf"),list(upper = Upper, median = Median, lower = Lower)))
a <- repro_my[repro_my$year == 18,]

repro_CI_CC <- repro %>% group_by (year, scenario) %>% summarise(across(c("mean_CI", "lower_CI", "upper_CI","mean_CC", "lower_CC", "upper_CC",),list( median = Median)))
a <- repro_CI_CC[repro_CI_CC$year == 18,]

# Box plot for reproductive performance per year ------
# label 
repro$scenario <- factor(repro$scenario, levels = c("1", "2", "3", "4","5"),
                         labels = c("Default", "FTAI", "FTAI+HD", "HD", "best"))

# plot repro from year 18 
# milk yield

ggplot(repro ,aes(x = as.factor(year), y = milk_yield/1000))+
  geom_boxplot()+
  labs(title="Annual milk yield",
       x ="Year", y = "Annual milk yield (tonnes)", fill ="Scenario")+
  facet_wrap(~scenario)+ facet_grid( cols = vars(scenario))+
  theme_bw() 

# number of cull cows
 ggplot(repro , aes(x = as.factor(year), y = nCulls))+
  geom_boxplot()+
  labs(title="Number of culled cows",
       x ="Year", y = "Number of culled cows", fill ="Scenario")+
  facet_wrap(~scenario)+ facet_grid( cols = vars(scenario))+
  theme_bw() 

# number of calves
ggplot(repro ,aes(x = as.factor(year), y = nCalf))+
  geom_boxplot()+
  labs(title="Number of calves",
       x ="Year", y = "Number of calves", fill ="Scenario")+
  facet_wrap(~scenario)+ facet_grid( cols = vars(scenario))+
  theme_bw()

# arrange grid
plot_grid(A1,A2, A3, labels = c('A', 'B', 'C'),ncol = 1)

##~~~~~~~~~~~~~~~~~~-------------------------------------------------------------
# Plot for culling ------
baseline_output_culling_best$cull==2
# all cows in baseline scenarios are getting culled from lower milk <15 because of n_insem and n_insem pregloss ==0 
summary(baseline_output_culling_best$par[baseline_output_culling_best$cull==2])
summary(baseline_output_culling_best$n_insem[baseline_output_culling_best$cull==2])
summary(baseline_output_culling_best$n_insem_pregloss[baseline_output_culling_best$cull==2])
##~~~~~~~~~~~~~~~~~~-------------------------------------------------------------

# Note -----
# get value on year 18th 
a <- Output.GHG.cowday$VEM.total == Output.GHG.cowday$VEM.roughage + Output.GHG.cowday$VEM.concentrate
b <- Output.GHG.cowday$VEM.maintain40 == Output.GHG.cowday$VEM.roughage + Output.GHG.cowday$VEM.concentrate
sum(a==b)

a <- GHG_sum_long %>% filter (year == 18) %>% filter (value %in% c("sum.GHG.system_tFPCM", "sum.GHG.feedLU_tFPCM", "sum.GHG.enteric_tFPCM","sum.GHG.manure_tFPCM","sum.GHG.substitute.cull_tFPCM", "sum.GHG.substitute.calves_tFPCM"))
rm(a)

summary(Estus_0.3scene3_output [Estus_0.3scene3_output$iter == 1 & Estus_0.3scene3_output$year == 20 , ])