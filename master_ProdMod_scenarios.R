setwd("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/Cow simulation")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load packages -----------------------------------------------------------

library(data.table)
library(profvis)
library(directlabels)
library(esquisse)
library(Hmisc)
library(varhandle)
library(tidyverse)
library(beepr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(mc2d)
library(devtools)


# Simulation model --------------------------------------------------------

mkFrames <- function(X = xPRM, initData_i = initData_I) {

 n <- with(X, ts*n_yrs+unique(initData_i$i))
 list2env(X, globalenv())
 
 # Object initialisation ---------------------------------------------------
 
 cowsList  <- vector(mode = "list", length = n) # list to collect daily data for cows
 cullsList <- vector(mode = "list", length = n) # list to collect cull data
 
 # protocol status (0 = no protocol; 1 = double ovsynch; 2 = ovsynch; 3 = PRID synch; 
 # 4 = open cows to be detected for heat (CevaScen 2:3))
 
 # initialise cow-spaces before simulation
 cow      <- as.list(initData_i[,-".id"])
 cullcows <- list() #~ used for validation purposes, collects culled cow data
 cow$p_conc <- rep(NA, n_cows) #this variable is added to calculate conception rate in master analysis
 
 # newCowLevelMS <- rep(1, n_cows)
 cow.entries   <- n_cows
 day           <- unique(initData_i$i)
 glb_PD        <- unique(initData_i$i) + 7 - (unique(initData_i$i) %% 7)  # global pregnancy diagnosis scheduling variable
 
 # initialise open cows to be checked at next veterinary visit
 # cow[["pregCheck_sdl"]][which(day - cow[["insem_day"]] <= 90)] <- glb_PD
 # cow[["pregCheck_sdl"]][which(cow[["pregnant"]] == 0 & cow[["dayendvwp"]] >= day)] <- glb_PD
 
 if(CEVA_scen %in% 1:2){
    # init0_idx <- which(cow[["pregnant"]] == 0 & (cow[["dayendvwp"]] - day) - cow[["dim"]] >= t_dblOvSynch)
    # init1_idx <- which(cow[["pregnant"]] == 0 & (cow[["dayendvwp"]] - day) - cow[["dim"]] < t_dblOvSynch)
   init0_idx <- which(cow[["pregnant"]] == 0 & cow[["dim"]] <= 49)
   init1_idx <- which(cow[["pregnant"]] == 0 & cow[["dim"]] >  49 & cow[["status"]] != 2)
    
    if(length(init0_idx) > 0 ){
       # cow[["oestrus"]][init0_idx] <- cow[["dayendvwp"]][init0_idx] + t_dblOvSynch2
       cow[["oestrus"]][init0_idx] <- (day + 1 + ((16 - ceiling((cow[["dim"]][init0_idx] + 1)/7)) * 7) -
         ((day+1) %% 7) -32) # ensuring 32d pregcheck post insemination; Double ovsynch protocol duration fixed at 27d; protocol start 50+-3d postpartum
       cow[["PTCL_status"]][init0_idx] <- 1
       cow[["cost.hormone"]][init0_idx] <- cost.hormone[1] # (search key: CostHormoneCeva)
    }
    if(length(init1_idx) > 0 ){
       cow[["oestrus"]][init1_idx]       <- glb_PD - 3 + t_dblOvSynch
       cow[["PTCL_status"]][init1_idx]   <- 1
       cow[["pregCheck_sdl"]][init1_idx] <- 0
       cow[["cost.hormone"]][init1_idx]  <- cost.hormone[1] # (search key: CostHormoneCeva)
       
       # for cows that are given a last chance to conceive
       # adjust daycullfert variable
       init2_idx <- init1_idx[which(cow[["daycullfert"]][init1_idx] - (day + 1) <= day - glb_PD + t_dblOvSynch)]  #
       if(length(init2_idx) > 0){
         cow[["daycullfert"]][init2_idx] <- cow[["oestrus"]][init2_idx]
       }
    }
    cow[["an_cyc1"]][cow[["an_cyc1"]] == 1] <- 0
    cow[["cod"]][cow[["cod"]] == 1]         <- 0
 }
 else{
    cow[["PTCL_status"]][which(cow[["pregnant"]] == 0)] <- 4
 }
 
 
 #~ Other initialisation processes to follow...
 
 # For loop ----------------------------------------------------------------  
 
 ptm <- proc.time()
 for(i in (day+1):n){
   
   # set.seed(i)
   day <- day + 1
   # model simulation starts on March 1st to account for seasonality
   season <- (ifelse(day %% 365 == 0, 365, day %% 365) %/% 92) + 1 
   
   cow[["day"]]  <- ifelse(cow[["day"]] == ts, 1, cow[["day"]] + 1) # update day in calendar year
   cow[["i"]]    <- i # rolling time steps
   cow[["year"]] <- ceiling(day/365)
   
   # schedule following pregnancy diagnosis
   if(day %% 7 == 0){glb_PD <- glb_PD + 7}
   
   #updating the day of pregnancy 
   cow[["daypregnant"]] <- ifelse(cow[["daypregnant"]] > 0, cow[["daypregnant"]] + 1, 0)
   
   # Variable reset ----------------------------------------------------------
   # For variables that have a one time value updated in the previous time step, they are reset here
   nodetect<- which(cow[["detoest"]] == 1)
   calfborn<- which(cow[['calf']] == 1)
   if(length(calfborn) > 0){
     cow[["prevdaycalf"]][calfborn] <- cow[["daycalf"]][calfborn]
     cow[["daycalf"]][calfborn] <- 0 
   }
   hormoneapplied <- which(cow[["cost.hormone"]] > 0)
   hormoneapplied.an <- which(cow[["cost.hormone.an"]] > 0)
   hormoneapplied.cod <- which(cow[["cost.hormone.cod"]] > 0)
   hormoneapplied.hi <- which(cow[["cost.hormone.hi"]] > 0)
   costlabour.ai        <- which(cow[["cost.labour.ai"]] > 0)
   costlabour.hu        <- which(cow[["cost.labour.hu"]] > 0)
   costPD <- which(cow[["cost.PD"]] != 0)
   costcalf        <- which(cow[["cost.calf"]] > 0)
   revcalf        <- which(cow[["rev.calf"]] > 0)
   suboestrus <- which(cow[["suboest"]] == 1)
   resetCows <- unique(c(nodetect, calfborn, hormoneapplied, 
                         hormoneapplied.an, hormoneapplied.cod,
                         hormoneapplied.hi, costlabour.ai, costlabour.hu, costPD, costcalf, revcalf, suboestrus))
   resetVars <- c("detoest", "cost.insem", "calf", 
                  "cost.hormone", "cost.hormone.an", "cost.hormone.cod", 
                  "cost.hormone.hi", "cost.PD", "cost.labour.ai", "cost.labour.hu", "cost.calf","rev.calf", "suboest")
   cow[resetVars] <- lapply(cow[resetVars], function(x) {x[resetCows] <- 0; x})
   
   
   
   # Culling and replacement -------------------------------------------------
   
   # replacement heifers will enter the herd the following day a cow as been removed from the milking herd  due to the above mentioned culling reasons
   culls1<- which(cow[["cull"]] %nin% c(0, 5)) 
   rhe<- which(cow[["cull"]] == 5 & day == cow[["rhenter"]])
   culls <- c(culls1, rhe)
   if(length(culls) > 0){
     
     if(validate == TRUE){
       # collect cull cow data 
       cullcows<- lapply(cow, "[", culls)
       cullcows[["cull"]]<- cow[["cull"]][culls]
       cullcows[["i"]]<- day
       cullsList[[i]]<- do.call(cbind, cullcows)
     }
     
     # reset variables in cow list
     z<- c("UID", "dim", "mdl", "yield", "yield.rdc", "rpl", "detoest", "pregnant", "calf", "cull", 
           "status", "oestrus", "daycalf", "daypregnant", "prevdaycalf", "dayendvwp", "daycullfert", 
           "n_insem", "n_insem_pregloss", "m_oest", "weight", "act.weight", "energy", "act.energy", "cost.insem", "cost.cull", "cost.cull.mort",
           "PTCL_status", "pregCheck_sdl", "an_cyc1", "cod", "insem_day", "treated") #~  (add variables as you bring them to life in the model)
     cow[z] <- lapply(cow[z], function(x) {x[culls] <- 0; x})
     
     # replacement heifer will enter milking herd on the assumption that cows to be culled are removed from the milking herd when a heifer is available
     z<- c("status", "par", "calf") # variables to reset with same value in cow list
     cow[z]<- lapply(cow[z], function(x) {x[culls] <- 1; x})
     cow[["mdl"]][culls]      <-  MDL_func(length(culls))
     cow[["rpl"]][culls]      <-  RPL(culls)
     cow[["dayendvwp"]][culls]<-  VWP + day + 1
     # cow[["oestrus"]][culls]  <-  sample(14:27, length(culls), replace = TRUE) + day
     cow[["weight"]][culls]   <- rnorm(length(culls), 540, 6)
     cow[["act.weight"]][culls] <- cow[["weight"]][culls] 
     # determine day which a cow can remain open until being flagged as a cull
     
     cow[["oestrus"]][culls]  <-  sample(t_ovu, length(culls), replace = TRUE) + day
     # for(cullsLoop in culls){
     #   cow[["daycullfert"]][cullsLoop] <- ifelse(cow[["rpl"]][cullsLoop] < daysOpen.mat$mean.rpl[1], cow[["oestrus"]][cullsLoop] + (21*2) + 1 - day, # accounting for cows with lowest RPL; 3 oestrus cycles; day is subtracted here since it is added at the end
     #                                             # assume 42 days (2 estrus cycles expected by the farmers) before the infertility culling decision
     #                                             ifelse(cow[["rpl"]][cullsLoop] > daysOpen.mat$mean.rpl[nrow(daysOpen.mat)], daysOpen.mat$days.open[nrow(daysOpen.mat)],
     #                                                    round(median(c(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cullsLoop])][length(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cullsLoop])])],
     #                                                                   daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl > cow[["rpl"]][cullsLoop])][1]), na.rm = TRUE)))) + day + 1
     # }
     
     
     
     # CEVA
     if(CEVA_scen %in% 1:2){
       cow[["PTCL_status"]][culls] <- 1
       
       if(day %% 7 == 0){
         cow[["oestrus"]][culls] <- day + (15*7) + (day %% 7) - 32 # ensuring 32d pregcheck post insemination; Double ovsynch protocol duration fixed at 27d; protocol start 50+-3d postpartum
       } else {
         cow[["oestrus"]][culls] <- day + ((16*7) - (day %% 7 * 2)) + (day %% 7) - 32 # ensuring 32d pregcheck post insemination; Double ovsynch protocol duration fixed at 27d; protocol start 50+-3d postpartum
       }
       
     }
     
     if(CEVA_scen == 3){
       cow[["oestrus"]][culls] <- day + sample(t_ovu, length(culls), replace = T)
       cow[["pregCheck_sdl"]][culls] <- cow[["dayendvwp"]][culls] + 24
       cow[["PTCL_status"]][culls] <- 4
       
       par_an   <- cow[["par"]][culls]; par_an[par_an > 1] <- 2
       yield_an <- rep(4, length(culls))
       yield_an[cow[["rpl"]][culls] <= yieldQuants[3]] <- 3 ; yield_an[cow[["rpl"]][culls] <= yieldQuants[2]] <- 2 ; yield_an[cow[["rpl"]][culls] <= yieldQuants[1]] <- 1
       
       pRC_cows <- RC_cows * anRF_par[par_an] * anRF_yield[yield_an] * anRF_season[rep(season, length(culls))] ; pRC_cows[pRC_cows > 1] <- 1
       
       rc_cows <- culls[rbinom(length(culls), 1, prob = pRC_cows) == 1] # ID cows that resume cyclicity
       an_cows <- setdiff(culls, rc_cows) # cows that are anoestrus
       
       par_cod    <- cow[["par"]][rc_cows] ; par_cod[par_cod > length(codRF_par)] <- length(codRF_par)
       seasonCalf <- (ifelse(day - cow[["dim"]][rc_cows] %% 365 == 0, 365, (day - cow[["dim"]][rc_cows]) %% 365) %/% 92) + 1
       pCOD_cows  <- COD_cows * codRF_par[par_cod] * codRF_season[seasonCalf] ; pCOD_cows[pCOD_cows > 1] <- 1
       if(any(is.na(rbinom(length(rc_cows), 1, prob = pCOD_cows)))) browser()
       cod_cows   <- rc_cows[rbinom(length(rc_cows), 1, prob = pCOD_cows) == 1]
       
       
       if(length(c(an_cows, cod_cows)) > 0){
         cow[["status"]][c(an_cows, cod_cows)]  <- 6  
         cow[["oestrus"]][c(an_cows, cod_cows)] <- 0
         
         cow[["an_cyc1"]][an_cows] <- 1
         cow[["cod"]][cod_cows]    <- 1
       }
     }
     
     # for replacement heifers that still have a culling decision day before the first oestrus, it is adjusted here
     # adjcull2 <- which(cow[["daycullfert"]][culls] <= (cow[["oestrus"]][culls] + (21*2)))
     # if(length(adjcull2) > 0){
     #   cow[["daycullfert"]][culls[adjcull2]] <-  (cow[["oestrus"]][culls[adjcull2]] + (21*2)) + 1
     # }
     
     #unique identification of cows
     cd1 <- paste0(paste0(rep(0, 5 - nchar(as.character(day))), sep="", collapse=""), day, "_" )
     cow[["UID"]][culls] <- sprintf("%s%0*d", cd1, 11 - nchar(cd1), cow.entries + 1:length(culls))
     cow.entries <- cow.entries + length(culls)
   }
   
   
   # Production --------------------------------------------------------------
   
   cow[["dim"]] <- cow[["dim"]] + 1
   # update oestrus cycles during VWP for healthy cows since farmer will not be interested in the oestrus cycles of a cow flagged to be culled
   oestwait <- which(cow[["oestrus"]] < cow[["dayendvwp"]] & cow[["status"]] != 6)
   oest_cyc <- sample(t_oest, length(oestwait), replace=T)
   if(length(oestwait) > 0){
     cow[["oestrus"]][oestwait]<- cow[["oestrus"]][oestwait] + oest_cyc
   }
   
   # when a cow is flagged for general culling before it has conceived, no need to find the day in which a fertility culling decision needs to be made
   nodaycf<- which(cow[["status"]] %in% c(4,5) & cow[["oestrus"]] >= cow[["dayendvwp"]])
   if(length(nodaycf) > 0){
     cow[["daycullfert"]][nodaycf] <- 0
   }
   
   # find cows in max parity since we are not interested in detecting for oestrus (maybe there is a neater way to do this..)
   mxp<- which(cow[["par"]] == max_par)
   if(length(mxp) > 0){
     cow[c("oestrus", "daycalf", "dayendvwp", "daycullfert")]<-  lapply(cow[c("oestrus", "daycalf", "dayendvwp", "daycullfert")], function(x) {x[mxp]<- 0; x})
   }
   
   # set the decision to stop insemination/detection
   # stop.insem <- which(cow[["n_insem"]] == max_insem)
   # #stop.insem2 <- which(cow[["yield"]] < yield_thresh.AI)
   # #stop.insem <- c(stop.insem1, stop.insem2)
   # if(length(stop.insem) > 0){
   #   cow[c("oestrus", "daycalf", "dayendvwp", "daycullfert")]<-  lapply(cow[c("oestrus", "daycalf", "dayendvwp", "daycullfert")], function(x) {x[stop.insem]<- 0; x})
   #   yield_below <- (which(cow[["yield"]] - cow[["yield.rdc"]] < yield_thresh))
   #   fert.cull <- intersect(stop.insem, yield_below)
   #   if(length(fert.cull) > 0){
   #     cow[["status"]][fert.cull] <- 2
   #     cow[["cull"]][fert.cull] <- 2
   #     cow[["daycullfert"]][fert.cull] <- day
   #     }
   # }
   
   # find day that cows goes into oestrus after VWP and healthy (i.e. cow[["status"]] == 1) cows the farmer would be willing to detect
   # do1<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["n_insem"]] + cow[["m_oest"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] + cow[["m_oest"]] <= max_insem_pregloss) #m_oest causes very short CI at output
   do1<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["n_insem"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   # do1<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["n_insem"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   # do1x<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["n_insem"]] <= max_insem )
   # do1y<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["yield"]] > yield_thresh.AI)
   # do1z<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   # do1 <- intersect(do1x, do1y,do1z)
   if(length(do1) > 0 & CEVA_scen == 0){
     # if(i == 188){browser()}
     #do1a <- do1[which(cow[["dim"]][do1] %in% VWP:(VWP+oest_cyc))]  # Find cows that are in first cycle after endVWP:
     do1a <- do1[which(cow[["dim"]][do1] > VWP & cow[["dim"]][do1] < cow[["oestrus"]][do1] & cow[["dim"]][do1] < VWP+26)]
     par_an   <- cow[["par"]][do1a] ; par_an[par_an > 1] <- 2 # and include cow-level risk factors for anoestrus
     yield_an <- rep(4, length(do1a))
     yield_an[cow[["rpl"]][do1a] <= yieldQuants[3]] <- 3 ; yield_an[cow[["rpl"]][do1a] <= yieldQuants[2]] <- 2 ; yield_an[cow[["rpl"]][do1a] <= yieldQuants[1]] <- 1
     seasonCalf <- (ifelse(day - cow[["dim"]][do1a] %% 365 == 0, 365, (day - cow[["dim"]][do1a]) %% 365) %/% 92) + 1 # season of calving will have to look back in time from today (i.e day - dim and sheck for season)
     
     pRC_cows <- RC_cows * anRF_par[par_an] * anRF_yield[yield_an] * anRF_season[season] ; pRC_cows[pRC_cows > 1] <- 1
     do1aa <- do1a[rbinom(length(do1a), 1, prob = pRC_cows) == 0] #probability of not resuming cyclicity
     if(length(do1aa) > 0){
       cow[["an_cyc1"]][do1aa] <- 1
     }
     do1ab <- setdiff(do1, do1aa) # cows that resume cyclicity after VWP
     do1ab <- do1ab[which(cow[["an_cyc1"]][do1ab] == 0 & cow[["cod"]][do1ab] == 0)]
     do1ab <- do1ab[rbinom(length(do1ab), 1, SUB_cows) == 1]
     if(length(do1ab) > 0 ){
       cow[["suboest"]][do1ab] <- 1
     }
     
     # incidence of COD cows
     do1b <- setdiff(do1, c(do1aa, do1ab))
     do1b <- do1b[which(cow[["dim"]][do1b] %in% VWP:pp_cod & cow[["cod"]][do1b] == 0 & 
                          cow[["an_cyc1"]][do1b] == 0 & cow[["suboest"]][do1b] == 0 &
                          cow[["treated"]][do1b] == 0)]
     
     par_cod <- cow[["par"]][do1b] ; par_cod[par_cod > length(codRF_par)] <- length(codRF_par)
     seasonCalf <- (ifelse(day - cow[["dim"]][do1b] %% 365 == 0, 365, (day - cow[["dim"]][do1b]) %% 365) %/% 92) + 1
     pCOD_cows <- COD_cows * codRF_par[par_cod] * codRF_season[seasonCalf] ; pCOD_cows[pCOD_cows > 1] <- 1
     do1b <- do1b[rbinom(length(do1b), 1, prob = pCOD_cows) == 1]
     if(length(do1b) > 0 ){
       cow[["cod"]][do1b] <- 1
     }
     
     do1c <- setdiff(do1, c(do1aa, do1ab, do1b)) # normal cycling cows (not affected by anoestrus or suboest or COD in this time step ; and cows that were treated `t_hormone`X`` days ago)
     
     # recovery from previous anoestrus 
     do1d <- do1c[which(cow[["an_cyc1"]][do1c] == 1 & cow[["treated"]][do1c] != 1)]
     do1d <- do1d[rbinom(length(do1d), 1, prob = sr_an) == 0] # non-recovered cows
     
     do1e <- do1c[which(cow[["cod"]][do1c] == 1 & cow[["treated"]][do1c] != 1)]
     do1e <- do1e[rbinom(length(do1e), 1, prob = sr_cod) == 0] # non-recovered cows
     
     do1c <- setdiff(do1c, c(do1d, do1e))    # all normal cycling cows and cows previously treated with hormone`X`
     do1f <- do1c[which(cow[["treated"]][do1c] == 1)]
     do1c <- setdiff(do1c, do1f)             # all normal cycling cows
     if(length(do1c) > 0){
       # predict detection of oestrus by binomial process and assume insemination occurs after successful detection
       # adjusting detection for production level (flv) and lactation stage (my6 and myJ) --- Inchaisri et al 2010
       flv <- rep(3, length(do1c)) 
       flv[cow[["rpl"]][do1c] <= 1.1] <- 2 ; flv[cow[["rpl"]][do1c] <= 0.9] <- 1
       flv2 <- cow[["par"]][do1c]; flv2[flv2 > 3] <- 3     #finding parity categories
       my6 <- (a[flv2] + b[flv2] * peakYieldDay[flv2] + d * exp(-K * peakYieldDay[flv2])) + cow[["rpl"]][do1c] * ady[flv2]
       myJ <- (a[flv2] + b[flv2] * cow[["dim"]][do1c] + d * exp(-K * cow[["dim"]][do1c])) + cow[["rpl"]][do1c] * ady[flv2]
       
       p_dtoes2 <- p_dtoes * flv * my6 / myJ; p_dtoes2[p_dtoes2 > 1] <- 1; p_dtoes2[p_dtoes2 < 0] <- 0
       
       cow[["detoest"]][do1c]<- rbinom(length(do1c), 1, prob = p_dtoes2)
       
       # for cows that spontaneously recovered from anoestrus or cod that were eligible for 
       # detection (i.e. cow in do1c), some may have been missed
       # then update an_cy1 and cod variables to represent spontaneous recovery
       # for cows that were detected after recovery, the value in an_cyc1 and cod are used as 
       # risk factors for conception and are reset after insemination 
       cow[["an_cyc1"]][ do1c[ cow[["detoest"]][do1c] == 0 & cow[["an_cyc1"]][do1c] == 1 ] ] <- 0
       cow[["cod"]][ do1c[ cow[["detoest"]][do1c] == 0 & cow[["cod"]][do1c] == 1 ] ] <- 0      
     }
     do1g <- do1f[which(apply(cbind(cow[["an_cyc1"]][do1f], cow[["cod"]][do1f]), 1,
                              function(j){any(j == 1)}))]
     if(length(do1g) > 0){ # cows that will be submitted to insemination because of hormone application
       cow[["detoest"]][do1g] <- 1
     }
     do1h <- setdiff(do1f, do1g) # all cows that were treated for heat induction
     if(length(do1h) > 0){
       cow[["detoest"]][do1h] <- rbinom(length(do1h), 1, prob = p_dtoesHI)
     }
     tmp_counter <- tmp_counter + 1
     # oestCheck[[tmp_counter]] <- c("All cows" = length(do1),
     #                               "PP an"   = length(do1[which(cow[["dim"]][do1] %in% VWP:(VWP+21))]),
     #                               "Cyc after VWP" = length(setdiff(do1, do1aa)),
     #                               "PP cod"  = length(setdiff(do1, c(do1aa, do1ab))[which(cow[["dim"]][setdiff(do1, c(do1aa, do1ab))] %in% VWP:pp_cod & cow[["cod"]][setdiff(do1, c(do1aa, do1ab))] == 0 & 
     #                                                               cow[["an_cyc1"]][setdiff(do1, c(do1aa, do1ab))] == 0 & cow[["suboest"]][setdiff(do1, c(do1aa, do1ab))] == 0 &
     #                                                               cow[["treated"]][setdiff(do1, c(do1aa, do1ab))] == 0)]),
     #                               "Inc an" = length(do1aa),
     #                               "Inc sub" = length(do1ab),
     #                               "Inc cod" = length(do1b),
     #                               "Normal cyc" = length(do1c),
     #                               "Unrecover an" = length(do1d),
     #                               "Unrecover cod" = length(do1e),
     #                               "Det norm cyc" = sum(cow$detoest[do1c] == 1),
     #                               "Sub treat" = length(do1h),
     #                               "Det sub treat" = sum(cow$detoest[do1h] == 1))
   }
   
   # do2x<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["PTCL_status"]] %in% 1:3 & cow[["n_insem"]] <= max_insem )
   # do2y<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["PTCL_status"]] %in% 1:3 & cow[["yield"]] > yield_thresh.AI)
   # do2z<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["PTCL_status"]] %in% 1:3 & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   # do2 <- c(do2x, do2y,do2z)
   # do2<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1, 6) & cow[["PTCL_status"]] %in% 1:3 & cow[["n_insem"]] + cow[["m_oest"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] + cow[["m_oest"]] <= max_insem_pregloss)
   do2<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1, 6) & cow[["PTCL_status"]] %in% 1:3 & cow[["n_insem"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   # do2<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1, 6) & cow[["PTCL_status"]] %in% 1:3 & cow[["n_insem"]] + cow[["m_oest"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] + cow[["m_oest"]] <= max_insem_pregloss)
   if(length(do2)  > 0 & CEVA_scen %in% 1:3){
     cow[["detoest"]][do2] <- 1
   }
   
   # do3x<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["PTCL_status"]] %in% 4 & cow[["n_insem"]] <= max_insem )
   # do3y<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["PTCL_status"]] %in% 4 & cow[["yield"]] > yield_thresh.AI)
   # do3z<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1) & cow[["PTCL_status"]] %in% 4 & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   # do3 <- c(do3x, do3y,do3z)
   # do3 <- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1, 6) & cow[["PTCL_status"]] %in% 4 & cow[["n_insem"]] + cow[["m_oest"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] + cow[["m_oest"]] <= max_insem_pregloss)
   do3<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1, 6) & cow[["PTCL_status"]] %in% 4 & cow[["n_insem"]] <= max_insem & cow[["yield"]] > yield_thresh.AI & cow[["n_insem_pregloss"]] <= max_insem_pregloss)
   if(length(do3)  > 0 & CEVA_scen %in% 2:3){
     
     flv <- rep(3, length(do3)) 
     flv[cow[["rpl"]][do3] <= 1.1] <- 2 ; flv[cow[["rpl"]][do3] <= 0.9] <- 1
     flv2 <- cow[["par"]][do3]; flv2[flv2 > 3] <- 3
     my6 <- (a[flv2] + b[flv2] * peakYieldDay[flv2] + d * exp(-K * peakYieldDay[flv2])) + cow[["rpl"]][do3] * ady[flv2]
     myJ <- (a[flv2] + b[flv2] * cow[["dim"]][do3] + d * exp(-K * cow[["dim"]][do3])) + cow[["rpl"]][do3] * ady[flv2]
     
     p_dtoes2 <- p_dtoes * flv * my6 / myJ; p_dtoes2[p_dtoes2 > 1] <- 1
     
     cow[["detoest"]][do3]<- rbinom(length(do3), 1, prob = p_dtoes2)
   }
   
   
   
   # CEVA | BASELINE oestrus detection --------------------------------------------------------------------------------------------------------------
   # where oestrus has been detected, count insemination 
   ins<- which(cow[["detoest"]] == 1 & cow[["n_insem_pregloss"]] == 0)
   if(length(ins) > 0){
     cow[["n_insem"]][ins]    <- cow[["n_insem"]][ins] + 1
     cow[["insem_day"]][ins]  <- day
     cow[["cost.insem"]][ins] <- cost.insem 
     cow[["cost.labour.ai"]][ins] <- cost_lab/60 * t_labAI
     
     cow[["pregCheck_sdl"]][ins] <- glb_PD + 4 * 7 # cows will have a pregnancy diagnosis in 29:35 days from insemination (32 plus minus 3 days)
   }
   
   ins_ploss <- which(cow[["detoest"]] == 1 & cow[["n_insem_pregloss"]] != 0)
   if(length(ins_ploss) > 0){
     cow[["n_insem_pregloss"]][ins_ploss]  <- cow[["n_insem_pregloss"]][ins_ploss] + 1
     # cow[["n_insem"]][ins_ploss]    <- cow[["n_insem_pregloss"]][ins_ploss]  #update the n insem to equal with n insem pregloss (if the pregloss occurs)
     cow[["n_insem"]][ins_ploss]    <- cow[["n_insem"]][ins_ploss] + 1 
     cow[["insem_day"]][ins_ploss]  <- day
     cow[["cost.insem"]][ins_ploss] <- cost.insem
     cow[["cost.labour.ai"]][ins_ploss] <- cost_lab/60 * t_labAI
     
     cow[["pregCheck_sdl"]][ins_ploss] <- glb_PD + 4 * 7 # cows will have a pregnancy diagnosis in 29:35 days from insemination (32 plus minus 3 days)
   }
   
   # CEVA | baseline
   # if(strict_pdINT){
   #   cow[["pregCheck_sdl"]][ins] <- t_obs + day
   # }else{
   #   if(glb_PD - day < 30){
   #     cow[["pregCheck_sdl"]][ins] <- glb_PD + 30
   #   }else{
   #     cow[["pregCheck_sdl"]][ins] <- glb_PD
   #   }
   # }
   
   
   # once cow is successfully detected/inseminated, predict conception by binomial process
   # by first finding successfully inseminated cows
   si<- which(cow[["detoest"]] == 1)
   if(length(si) > 0){
     
     #include cow-level risk factors for conception rate
     par_cr <- cow[["par"]][si] ; par_cr[par_cr > 1] <- 2
     cyc_cr <- cow[["an_cyc1"]][si] + 1
     cod_cr <- cow[["cod"]][si] + 1 
     soc_cr <- (ifelse((day - cow[["dim"]][si]) %% 365 == 0, 365, (day - cow[["dim"]][si]) %% 365) %/% 92) + 1
     
     # once risk factors have been found, reset `an_cyc1` and `cod` variables because if the cow has been detected it means that it has recovered (either spontaneously or after treatment)
     cow[["an_cyc1"]][si[cow[["an_cyc1"]][si] == 1]] <- 0
     cow[["cod"]][si[cow[["cod"]][si] == 1]]         <- 0 
     
     
     cow$p_conc[!is.na(cow$p_conc)] <- NA #to calculate the conception rate in master analysis
     if(CEVA_scen %in% c(0, 3)){
       
       #find cows treated with hormone (this will only occur for the baseline_int scenario)
       si3 <- si[which(cow[["treated"]][si] == 1)]  # cows previously treated with hormones
       si2 <- setdiff(si, si3)                      # cows not previously treated with hormones
       sc2 <- sc3 <- integer()
       
       
       if(length(si2) > 0){
         #browser()
         #n_ins <- cow[["n_insem"]][si2] ; n_ins[n_ins > 6] <- 6
         #prob_conc <- (p_conc[n_ins] * calib_CR) * conRF_par[par_cr[si %in% si2]] * conRF_cyc[cyc_cr[si %in% si2]] * conRF_cod[cod_cr[si %in% si2]] * conRF_soc[soc_cr[si %in% si2]] ; prob_conc[prob_conc > 1] <- 1
         
         #include cow-level characteristic variables for conception rate (Inchaisri et al 2011, Table 2)
         n_ins  <- cow[["n_insem"]][si2]; n_ins[n_ins > 6]   <- 6
         par_cr <- cow[["par"]][si2]    ; par_cr[par_cr > 5] <- 5
         
         tmp_par <- cow[["par"]][si2]  #to get 3 parities based on xPRM$peakYieldDay
         tmp_par[tmp_par > 3] <- 3
         ins_rel_py <- ((cow[["dim"]][si2]) > peakYieldDay[tmp_par]) + 1 #AI time related to peak milk time
         
         #model the logistic regression
         y = cr_intc + cr_AI[n_ins] + cr_par[par_cr] + cr_breed + cr_lastcalf
         + cr_AIseason[season] + cr_peak[ins_rel_py] + (cow[["dim"]][si2] * cr_DIM) 
         + ((cow[["yield"]][si2] - cow[["yield.rdc"]][si2]) * cr_MY) 
         + (cow[["dim"]][si2] * (cow[["yield"]][si2] - cow[["yield.rdc"]][si2]) * cr_MY.DIM)
         + (cow[["dim"]][si2] * cr_season.DIM[season]) + (cow[["dim"]][si2] * cr_AI.DIM[n_ins])
         + cow[["dim"]][si2] * cr_par.DIM[par_cr]
         
         p_conc <- exp(y)/(1+exp(y))
         
         prob_conc <- p_conc * conRF_cyc[cyc_cr[si %in% si2]] * conRF_cod[cod_cr[si %in% si2]]; prob_conc[prob_conc > 1] <- 1
         sc2 <- si2[rbinom(length(si2), 1, prob = prob_conc) == 1] # successful conception
         cow$p_conc[si2] <- prob_conc
       }
       if(length(si3) > 0){
         #if(CEVA_scen == 3){
         #print(paste("si3 cows picked up in scenario 3, year =", unique(cow[["year"]])))
         #       }
         si3.an  <- si3[which(cow[["an_cyc1"]][si3] == 1)] ; names(si3.an) <- rep(1, length(si3.an))
         si3.cod <- si3[which(cow[["cod"]][si3] == 1)]     ; names(si3.cod) <- rep(2, length(si3.cod))
         si3.sub <- setdiff(si3, c(si3.an, si3.cod))       ; names(si3.sub) <- rep(3, length(si3.sub))
         
         treat_idx <- as.numeric(names(sort(c(si3.an, si3.cod, si3.sub))))
         #prob_conc <- p_concH[treat_idx] * conRF_par[par_cr[si %in% si3]] * conRF_cyc[cyc_cr[si %in% si3]] * conRF_cod[cod_cr[si %in% si3]] * conRF_soc[soc_cr[si %in% si3]] ; prob_conc[prob_conc > 1] <- 1
         prob_conc <- p_concH[treat_idx]; prob_conc[prob_conc > 1] <- 1
         sc3 <- si3[rbinom(length(si3), 1, prob = prob_conc) == 1] # successful conception
         cow[["treated"]][si3]     <- 0
         cow[["an_cyc1"]][si3.an]  <- 0
         cow[["cod"]][si3.cod]     <- 0
       }
       sc <- c(sc2, sc3) # cows that conceived based on n_insem (sc2) and based on previous treatment (sc3)
       # if(any(is.na(sc))) browser()      
     }
     
     
     if(CEVA_scen %in% 1:2){
       ptcl_sts <- cow[["PTCL_status"]][si]
       #if(4 %in% ptcl_sts){
       #   print("there is a ptcl 4")
       #}
       prob_conc <- p_conc[ptcl_sts] * conRF_par[par_cr] * conRF_cod[cod_cr] * conRF_cyc[cyc_cr] * conRF_soc[soc_cr] ; prob_conc[prob_conc > 1] <- 1
       #prob_conc <- p_conc[ptcl_sts] ; prob_conc[prob_conc > 1] <- 1
       sc <- si[rbinom(length(si), 1, prob = prob_conc) == 1] # successful conception
       
       
       if(CEVA_scen == 2){
         # find non-pregnant cows
         sc2 <- setdiff(si, sc)
         if(length(sc2) > 0){
           
           cow[["oestrus"]][sc2]     <- day + sample(t_oest, length(sc2), replace = T)
           cow[["PTCL_status"]][sc2] <- 4
           
           sc3 <- sc2[cow[["status"]][sc2] == 6]
           if(length(sc3) > 0){
             cow[["status"]][sc3] <- 1 
           }
         }
       }
     }
     
     # tag cow as pregnant after successful conception
     if(length(sc) > 0){
       cow[["pregnant"]][sc]    <- 1
       cow[["status"]][sc]      <- 1 
       cow[["daycalf"]][sc]     <- round(rnorm(length(sc), 281, 3)) + day # predict day of calving (calibrated to Inchaisri)
       cow[["daypregnant"]][sc] <- 1
       cow[["mdl"]][sc]         <- ((cow[["daycalf"]][sc] - DPL - day) - (cow[["mdl"]][sc] - cow[["dim"]][sc])) + cow[["mdl"]][sc] - 1 # adjust number of milking days to achieve fixed DPL
       cow[["daycullfert"]][sc] <- 0 # no culling day decision since cow is pregnant
       cow[["an_cyc1"]][sc]     <- 0  
       cow[["cod"]][sc]         <- 0
     }
   }
   
   #reset day of pregnancy after the cow get pregnancy loss
   # cow[["daypregnant"]] <- ifelse(cow[["pregloss"]] > 0, 0, cow[["daypregnant"]])
   
   cpregloss <- which(cow[["pregloss"]] == 1)
   if(length(cpregloss) > 0){
     cow[["pregnant"]][cpregloss]<- 0
     cow[["daypregnant"]][cpregloss]<- 0
     cow[["pregloss"]][cpregloss]<- 0
     cow[["daycalf"]][cpregloss]<- 0
     cow[["oestrus"]][cpregloss]<- day + sample(t_oest, length(cpregloss), replace=T)
     #cow[["n_insem"]][cpregloss]<- 1 #to ensure that the n insem equal with n insem pregloss (if the pregloss occurs)
     cow[["n_insem_pregloss"]][cpregloss]<- 1
     cow[["n_insem"]][cpregloss] <- cow[["n_insem"]][cpregloss] + cow[["n_insem_pregloss"]][cpregloss] #add n insem with n insem pregloss (if the pregloss occurs)
     # cow[["m_oest"]][cpregloss]<- 0
     
     if(CEVA_scen %in% 1:3){
       
       if(CEVA_scen %in% 1:2){
         `CL+` <- `cpregloss`[rbinom(length(`cpregloss`), 1, prob = `p_CL+`) == 1]
         `CL-` <- setdiff(`cpregloss`, `CL+`) 
         AN    <- integer(0)
       }
       
       if(CEVA_scen %in% 3){
         AN    <- `cpregloss`[cow[["status"]][`cpregloss`] == 6]                    # find annoestrus cows that are checked after oestrus observation
         if(length(AN) > 0){
           `cpregloss` <- `cpregloss`[-AN]                                            # keep non-preg for corpus let. determination (after findin AN cows)
         }
         `CL+` <- `cpregloss`[rbinom(length(`cpregloss`), 1, prob = `p_CL+`) == 1]  # determine cows that have corpus let.
         `CL-` <- setdiff(`cpregloss`, `CL+`)                                 # determine cows that don't have corpus let.
       }
       
       if(length(`CL+`) > 0){
         cow[["PTCL_status"]][`CL+`] <- 2
         cow[["oestrus"]][`CL+`] <- day + t_OvSynch
         cow[["cost.hormone"]][`CL+`] <- cost.hormone[2]  # (search key: CostHormoneCeva)
         cow[["cost.labour.hu"]][`CL+`]  <- t_labOvSynch/60 * cost_lab 
       }
       
       if(length(`CL-`) > 0){
         cow[["PTCL_status"]][`CL-`] <- 3
         cow[["oestrus"]][`CL-`] <- day + t_PridSynch
         cow[["cost.hormone"]][`CL-`] <- cost.hormone[3] # (search key: CostHormoneCeva)
         cow[["cost.labour.hu"]][`CL-`]  <- t_labOvSynch/60 * cost_lab           
       }
       # for cows that are given a last chance to conceive
       # adjust daycullfert variable
       `ploss_DG-` <- c(`CL+`, `CL-`, AN)[which(cow[["daycullfert"]][c(`CL+`, `CL-`, AN)] - day <= 
                                                  cow[["oestrus"]][c(`CL+`, `CL-`, AN)] - day &
                                                  cow[["status"]][c(`CL+`, `CL-`, AN)] != 2 )]
       if(length(`ploss_DG-`) > 0){
         cow[["daycullfert"]][`ploss_DG-`] <- cow[["oestrus"]][`ploss_DG-`]
       }
       
       # DGpre_ploss1 <- which(cow[["n_insem"]] + cow[["m_oest"]] == max_insem & cow[["pregnant"]] == 0)
       DGpre_ploss1 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["n_insem_pregloss"]] >= max_insem_pregloss)
       DGpre_ploss2 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["dim"]] > 20 & cow[["yield"]] < yield_thresh.AI)
       DGpre_ploss <- c(DGpre_ploss1, DGpre_ploss2)
       if(length(DGpre_ploss) > 0){
         cow[["PTCL_status"]][DGpre_ploss] <- 0
         cow[["status"]][DGpre_ploss]      <- 2
       }
     }
     }
   
   #reset pregnancy loss after the day of loss pregnancy
   #cow[["pregloss"]] <- ifelse(cow[["pregloss"]] == 1, 0, cow[["pregloss"]])
   
   #model pregnancy loss after conception
   cowpregloss <- which(cow[["daypregnant"]] < 280 & cow[["daypregnant"]] > 60)
   if(length(cowpregloss) > 0){
     cow[["pregloss"]][cowpregloss] <- rbinom(length(cowpregloss), 1, prob = p_pregloss/(279-60))
   } 
   
   #cow[["pregloss"]] <- ifelse(cow[["daypregnant"]] < 256 & cow[["daypregnant"]] > 60, (rbinom(length(1), 1, prob = p_pregloss/(255-60))), cow[["pregloss"]])
   # cow[["pregloss"]] <- ifelse(cow[["daypregnant"]] < 31 & cow[["daypregnant"]] != 0, (rbinom(length(1), 1, prob = p_pregloss1)), cow[["pregloss"]])
   # cow[["pregloss"]] <- ifelse(cow[["daypregnant"]] < 46 & cow[["daypregnant"]] > 30, (rbinom(length(1), 1, prob = p_pregloss2)), cow[["pregloss"]])
   # cow[["pregloss"]] <- ifelse(cow[["daypregnant"]] < 181 & cow[["daypregnant"]] > 45, (rbinom(length(1), 1, prob = p_pregloss3)), cow[["pregloss"]])
   # cow[["pregloss"]] <- ifelse(cow[["daypregnant"]] < 256 & cow[["daypregnant"]] > 180, (rbinom(length(1), 1, prob = p_pregloss4)), cow[["pregloss"]])
   
   # BASELINE reproductive disorder diagnosis --------------------------------
   if(baseline_int & CEVA_scen == 0){
     rdd_idx <- which((cow[["pregnant"]] == 0 & cow[["n_insem"]] == 0 & cow[["dim"]] >= VWP + noinsem_int + 1 & cow[["treated"]] == 0) & cow[["pregCheck_sdl"]] != day | 
                        (cow[["pregnant"]] == 0 & day - cow[["insem_day"]] >= noinsem_int2 + 1 & cow[["n_insem"]] > 0 & cow[["treated"]] == 0  & cow[["pregCheck_sdl"]] != day)) 
     if(length(rdd_idx) > 0) {
       cow[["pregCheck_sdl"]][rdd_idx] <- glb_PD 
     }
   }    
   
   # CEVA | BASELINE pregnancy diagnosis and protocol selection -------------------------------------------------------------------------------------
   
   DGpre_idx <- which(cow[["pregCheck_sdl"]] == day)
   if(length(DGpre_idx) > 0){
     # scenario 1:3
     if(CEVA_scen %in% 1:3){
       `DG+` <- DGpre_idx[cow[["pregnant"]][DGpre_idx] == 1] # pregnant cows after diagnosis
       `DG-` <- setdiff(DGpre_idx, `DG+`)                    # non-pregnant cows after diagnosis
       `DG-` <- `DG-`[cow[["status"]][`DG-`] != 2 ]          # non-pregnant cows that can still be bred
       
       # update variable for pregnant cows
       if(length(`DG+`) > 0){
         cow[["pregCheck_sdl"]][`DG+`] <- 0
         cow[["PTCL_status"]][`DG+`] <- 0
       }
       
       # update variables for non-pregnant cows 
       if(length(`DG-`) > 0){
         if(CEVA_scen %in% 1:2){
           `CL+` <- `DG-`[rbinom(length(`DG-`), 1, prob = `p_CL+`) == 1]
           `CL-` <- setdiff(`DG-`, `CL+`) 
           AN    <- integer(0)
         }
         
         if(CEVA_scen %in% 3){
           AN    <- `DG-`[cow[["status"]][`DG-`] == 6]                    # find annoestrus cows that are checked after oestrus observation
           if(length(AN) > 0){
             `DG-` <- `DG-`[-AN]                                            # keep non-preg for corpus let. determination (after findin AN cows)
           }
           `CL+` <- `DG-`[rbinom(length(`DG-`), 1, prob = `p_CL+`) == 1]  # determine cows that have corpus let.
           `CL-` <- setdiff(`DG-`, `CL+`)                                 # determine cows that don't have corpus let.
           # `CL-` <- c(`CL-`, AN)                                          # combine CL- and AN cows because same protocol is issued to these cows
           
         }
         
         if(length(`CL+`) > 0){
           cow[["PTCL_status"]][`CL+`] <- 2
           cow[["oestrus"]][`CL+`] <- day + t_OvSynch
           cow[["cost.hormone"]][`CL+`] <- cost.hormone[2]  # (search key: CostHormoneCeva)
           cow[["cost.labour.hu"]][`CL+`]  <- t_labOvSynch/60 * cost_lab 
         }
         
         if(length(`CL-`) > 0){
           cow[["PTCL_status"]][`CL-`] <- 3
           cow[["oestrus"]][`CL-`] <- day + t_PridSynch
           cow[["cost.hormone"]][`CL-`] <- cost.hormone[3] # (search key: CostHormoneCeva)
           cow[["cost.labour.hu"]][`CL-`]  <- t_labOvSynch/60 * cost_lab           
         }
         
         if(length(AN) > 0){
           cow[["PTCL_status"]][AN] <- 3
           cow[["oestrus"]][AN] <- day + t_PridSynch
           cow[["cost.hormone"]][AN] <- cost.hormone[3] # (search key: CostHormoneCeva)
           cow[["cost.labour.hu"]][AN]  <- t_labOvSynch/60 * cost_lab     
         }
         # for cows that are given a last chance to conceive
         # adjust daycullfert variable
         `tmp_DG-` <- c(`CL+`, `CL-`, AN)[which(cow[["daycullfert"]][c(`CL+`, `CL-`, AN)] - day <= 
                                                  cow[["oestrus"]][c(`CL+`, `CL-`, AN)] - day &
                                                  cow[["status"]][c(`CL+`, `CL-`, AN)] != 2 )]
         if(length(`tmp_DG-`) > 0){
           cow[["daycullfert"]][`tmp_DG-`] <- cow[["oestrus"]][`tmp_DG-`]
         }
       }
       DGpre_idx2 <- which(cow[["pregCheck_sdl"]] == day & cow[["pregnant"]] == 0 & cow[["daycullfert"]] <= day)
       if(length(DGpre_idx2) > 0){
         cow[["PTCL_status"]][DGpre_idx2] <- 0
         cow[["status"]][DGpre_idx2]      <- 2
       }
     }
     
     if(CEVA_scen == 0 & baseline_int){ # first anoestrus and cod cows are diagnosed, 
       # then heat induction for cows that are normally cycling but just missed or silent for `noinsem_int` days
       DGpre_idx2 <- DGpre_idx[apply(cbind(cow[["an_cyc1"]][DGpre_idx], cow[["cod"]][DGpre_idx]), 1,   # find anoestrus or cod positive cows 
                                     function(j){any(j == 1)})]
       if(length(DGpre_idx2) > 0){
         DGpre_idx3 <- DGpre_idx2[which(cow[["an_cyc1"]][DGpre_idx2] == 1)]   #find anoestrus cows
         DGpre_idx4 <- setdiff(DGpre_idx2, DGpre_idx3)                        #find COD cows
         
         cow[["oestrus"]][DGpre_idx3] <- day + t_hormone_an 
         cow[["oestrus"]][DGpre_idx4] <- day + t_hormone_cod 
         
         cow[["treated"]][c(DGpre_idx3, DGpre_idx4)] <- 1 
         cow[["treat_day"]][c(DGpre_idx3, DGpre_idx4)] <- day 
         cow[["pregCheck_sdl"]][c(DGpre_idx3, DGpre_idx4)] <- 0
         
         # calculate treatment costs
         cow[["cost.hormone.an"]][DGpre_idx3] <- cost.hormone.an
         cow[["cost.hormone.cod"]][DGpre_idx4] <- cost.hormone.cod
         cow[["cost.labour.hu"]][c(DGpre_idx3)]  <- t_labPridSynchAN/60 * cost_lab     
         cow[["cost.labour.hu"]][c(DGpre_idx4)]  <- t_labOvSynchCOD/60 * cost_lab    
         
       }
       DGpre_idx5 <- setdiff(DGpre_idx, DGpre_idx2)
       DGpre_idx5 <- DGpre_idx5[which(cow[["pregnant"]][DGpre_idx5] != 1 & day - cow[["insem_day"]][DGpre_idx5] >= 30)]  # find cows that were either sub-oestrus or missed in the previous `noinsem_int` days 
       DGpre_idx5 <- DGpre_idx5[rbinom(length(DGpre_idx5), 1, p_subselect) == 1] #sub-estrus cow that got heat induction
       if(length(DGpre_idx5) > 0){
         cow[["treated"]][DGpre_idx5] <- 1
         cow[["insem_day"]][DGpre_idx5] <- day  # we use treat_day as a variable to also determine the day on which a cow was treated for missed or sub-oestrus (it helps with the intervention criteria)
         cow[["pregCheck_sdl"]][DGpre_idx5] <- 0
         #calculate treatment costs
         cow[["cost.hormone.hi"]][DGpre_idx5] <- cost.hormone.hi
         cow[["cost.labour.hu"]][DGpre_idx5]  <- t_labHI/60 * cost_lab  
       }
     }    
     # Preg diagnosis costs (search key: CostPregDiag)
     cow[["cost.PD"]][DGpre_idx] <- cpd_cof/length(DGpre_idx) + lpd_dur/60 * cpd_lb1 +  lpd_ftc/60/length(DGpre_idx) * cpd_lb0
   }
   
   
   
   # where oestrus has not been detected or unsuccessful insemination occurred for cows that have not yet been flagged for culling reasons (=2 etc.), add another oestrus cycle
   missedoest<- which(cow[["status"]] %in% c(1) & cow[["oestrus"]] == day & cow[["pregnant"]] == 0  & CEVA_scen %nin% c(1,2,3))   # detected and not pregnant
   oest_cyc2 <- sample(t_oest, length(missedoest), replace=T)
   if(length(missedoest) > 0){
     cow[["oestrus"]][missedoest] <- cow[["oestrus"]][missedoest] + oest_cyc2 
     cow[["treated"]][missedoest] <- 0
     missedoest <- missedoest[which(cow[["detoest"]][missedoest] == 0)]
     cow[["m_oest"]][missedoest] <- cow[["m_oest"]][missedoest] + 1 
   }
   # missedoest2 <- which(cow[["detoest"]][missedoest] == 0) # not detected 
   missedoest2 <- which(cow[["status"]] %in% c(1, 6) & cow[["oestrus"]] == day & cow[["pregnant"]] == 0 & cow[["PTCL_status"]] == 4)
   oest_cyc3 <- sample(t_oest, length(missedoest2), replace=T)
   if(length(missedoest2) > 0){
     cow[["oestrus"]][missedoest2] <- cow[["oestrus"]][missedoest2] + oest_cyc3 
     cow[["m_oest"]][missedoest2] <- cow[["m_oest"]][missedoest2] + 1
   }
   
   
   
   
   # Calving -----------------------------------------------------------------
   
   # find the day of calving; this will also be first day of milking
   cd<- which(cow[["daycalf"]] == day)
   if(length(cd) > 0){
     cow[["calf"]][cd]<- rbinom(length(cd), 1, 1-p_stlbrn)
     cow[["pregnant"]][cd]<- 0
     cow[["daypregnant"]][cd]<- 0
     # find cows that have started milking to predict number of days until first heat cycle (order of oestrus and oestrus detection is important)
     cow[["dayendvwp"]][cd]<- VWP + day + 1 # find the first day after the end of the voluntary waiting period
     # cow[["oestrus"]][cd[cow[["par"]][cd] == 1]]<- sample(14:27, length(cd[cow[["par"]][cd] == 1]), replace = TRUE) + day # find day that cow will come into first oestrus (parity 1) (this is probably obsolete here since this is run when a replacement heifer enters the herd above)
     # cow[["oestrus"]][cd[cow[["par"]][cd] != 1]]<- sample(18:21, length(cd[cow[["par"]][cd] != 1]), replace = TRUE) + day # find day that cow will come into first oestrus (parity 2+)
     cow[["par"]][cd]<- cow[["par"]][cd] + 1
     cow[["dim"]][cd]<- 1
     cow[["mdl"]][cd]<- MDL_func(cd)
     
     
     # determine day which day a cow can remain open until being flagged as a cull
     #~ if using: Remember to set the n_cows in the `daysOpen_func.R` to your desired herd size
     
     cow[["oestrus"]][cd[cow[["par"]][cd] == 2]] <- sample(t_ovu, length(cd[cow[["par"]][cd] == 2]), replace = TRUE) + day # find day that cow will come into first oestrus (parity 1) (this is probably obsolete here since this is run when a replacement heifer enters the herd above)
     cow[["oestrus"]][cd[cow[["par"]][cd] > 2]]  <- sample(t_ovu, length(cd[cow[["par"]][cd] > 2]), replace = TRUE) + day # find day that cow will come into first oestrus (parity 2+)
     for(cdLoop in cd){
       cow[["daycullfert"]][cdLoop] <- ifelse(cow[["rpl"]][cdLoop] < daysOpen.mat$mean.rpl[1], cow[["oestrus"]][cdLoop] + (21*2) + 1 - day, # accounting for cows with lowest RPL; 3 oestrus cycles; day is subtracted here since it is added at the end
                                              ifelse(cow[["rpl"]][cdLoop] > daysOpen.mat$mean.rpl[nrow(daysOpen.mat)], daysOpen.mat$days.open[nrow(daysOpen.mat)],
                                                     round(median(c(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cdLoop])][length(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cdLoop])])],
                                                                    daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl > cow[["rpl"]][cdLoop])][1]), na.rm = TRUE)))) + day + 1
     }
     # # for cows that still have a culling decision day before the first oestrus, it is adjusted here
     adjcull <- which(cow[["daycullfert"]][cd] <= (cow[["oestrus"]][cd] + (21*2)))
     if(length(adjcull) > 0){
       cow[["daycullfert"]][cd[adjcull]] <-  (cow[["oestrus"]][cd[adjcull]] + (21*2)) + 1
     }
     
     #CEVA
     if(CEVA_scen %in% 1:2){
       # browser()
       # cow[["oestrus"]][cd] <- cow[["dayendvwp"]][cd] + t_dblOvSynch2 - 2  # -2 to get the 77 days on calving to 1st AI with Double ovsynch FTAI protocol
       cow[["PTCL_status"]][cd] <- 1
       
       if(day %% 7 == 0){
         cow[["oestrus"]][cd] <- day + (15*7) + (day %% 7) - 32 # ensuring 32d pregcheck post insemination; Double ovsynch protocol duration fixed at 27d; protocol start 50+-3d postpartum
       } else {
         cow[["oestrus"]][cd] <- day + ((16*7) - (day %% 7 * 2)) + (day %% 7) - 32 # ensuring 32d pregcheck post insemination; Double ovsynch protocol duration fixed at 27d; protocol start 50+-3d postpartum
       }
       
     }
     
     if(CEVA_scen %in% 3){
       cow[["oestrus"]][cd] <- day + sample(t_ovu, length(cd), replace = T)
       cow[["pregCheck_sdl"]][cd] <- cow[["dayendvwp"]][cd] + 24
       cow[["PTCL_status"]][cd] <- 4
       
       #risk factors re. resuming cyclicity 
       par_an   <- cow[["par"]][cd]; par_an[par_an > 1] <- 2
       yield_an <- rep(4, length(cd))
       yield_an[cow[["rpl"]][cd] <= yieldQuants[3]] <- 3 ; yield_an[cow[["rpl"]][cd] <= yieldQuants[2]] <- 2 ; yield_an[cow[["rpl"]][cd] <= yieldQuants[1]] <- 1
       pRC_cows <- RC_cows * anRF_par[par_an] * anRF_yield[yield_an] * anRF_season[rep(season, length(cd))] ; pRC_cows[pRC_cows > 1] <- 1
       
       rc_cows <- cd[rbinom(length(cd), 1, prob = pRC_cows) == 1] # ID cows that resume cyclicity
       an_cows <- setdiff(cd, rc_cows) # cows that are anoestrus
       
       
       par_cod    <- cow[["par"]][rc_cows] ; par_cod[par_cod > length(codRF_par)] <- length(codRF_par)
       seasonCalf <- (ifelse(day - cow[["dim"]][rc_cows] %% 365 == 0, 365, (day - cow[["dim"]][rc_cows]) %% 365) %/% 92) + 1
       pCOD_cows  <- COD_cows * codRF_par[par_cod] * codRF_season[seasonCalf] ; pCOD_cows[pCOD_cows > 1] <- 1
       if(any(is.na(rbinom(length(rc_cows), 1, prob = pCOD_cows)))) browser()
       cod_cows   <- rc_cows[rbinom(length(rc_cows), 1, prob = pCOD_cows) == 1]
       
       
       if(length(c(an_cows, cod_cows)) > 0){
         cow[["status"]][c(an_cows, cod_cows)]  <- 6  
         cow[["oestrus"]][c(an_cows, cod_cows)] <- 0
         
         cow[["an_cyc1"]][an_cows] <- 1
         cow[["cod"]][cod_cows]    <- 1
       }
       
     }
     
     # reset insemination count for following lactation
     cow[["n_insem"]][cd] <- 0
     cow[["m_oest"]][cd]  <- 0
     cow[["n_insem_pregloss"]][cd] <- 0
     
     # calculate costs of calving for next 14 days, unless removed earlier due to death
     #cd2 <- rbinom(length(cd), 1, prob = p_mortC) # these calves will die in the next 14 days
     cd2a <- rbinom(length(cd), 1, prob = p_mortC[1]) # these calves will die in the first day
     cd2b_idx <-  which(cd2a == 0)[rbinom(sum(cd2a == 0), 1, p_mortC[2]) == 1]# these calves will die in the 2-14 days
     #cd3 <- rep(14, length(cd)) ; cd3[cd2 == 1] <- sample(1:14, sum(cd2), replace = T) # define duration of calf in cattle model
     cd3 <- rep(14, length(cd)) ; cd3[cd2a == 1] <- 1; cd3[cd2b_idx] <- sample(2:14, length(cd2b_idx), replace = T) # define duration of calf in cattle model
     cd2a[cd2b_idx] <- 1 
     cd4 <- eval(WAB) ; names(cd4)     
     cd5 <- rep(2, length(cd4)) ; cd5[cd4 <= bw_milkreplace] <- 1
     
     cow[["cost.calf"]][cd] <- cost_calfmanage + (cd3 * t_labcalf[season]/60 * cost_lab) + (cd3 * l_milkreplace[cd5] * cost_milkreplace) + (cd2a * rem_calf)
     cow[["rev.calf"]][cd[cd3 == 14]] <- calf.revenue
   }
   
   # Lactation ---------------------------------------------------------------
   
   # stop milking after last milking day (will probably have to include something with yield and culling etc. afterwards)
   stopmilk<- which(cow[["dim"]] > cow[["mdl"]])
   if(length(stopmilk) > 0){
     cow[["yield"]][stopmilk]     <- 0
     cow[["yield.rdc"]][stopmilk] <- 0
   }
   
   # continue milking while cow is still in milk
   inmilk<- which(cow[["dim"]] <= cow[["mdl"]])
   if(length(inmilk) > 0){
     parCATdim <- cow[["par"]][inmilk] ; parCATdim[parCATdim > 3] <- 3
     # estimate milk yield (not yet corrected)
     cow[["yield"]][inmilk] <- a[parCATdim] + b[parCATdim] * cow[["dim"]][inmilk] + 
       d * exp(-K * cow[["dim"]][inmilk]) + cow[["rpl"]][inmilk] * ady[parCATdim]
   }
   
   
   # Average daily weight gain
   bwg  <- which(cow[["par"]] < 3)
   if(length(bwg) > 0){
     cow[["weight"]][bwg] <- cow[["weight"]][bwg] + ADG 
   }
   
   # Energy requirements
   
   # maintenance and FPCM
   EparCat <- cow[["par"]] ; EparCat <- EparCat[EparCat > 3] <- 3
   cow[["energy"]] <- 42.4 * cow[["weight"]]^0.75 + 442 * (cow[["yield"]] * 0.337 + 0.116 * fat[EparCat]*100 + 0.06 * prot[EparCat]*100)
   
   # growth
   Eg <- which(cow[["par"]] < 3)
   if(length(Eg) > 0){
     cow[["energy"]][Eg] <- cow[["energy"]][Eg] + G.ENGY[cow[["par"]][Eg]]
   }
   
   # stage of pregnancy
   Ep <- which(cow[["pregnant"]] == 1 & (cow[["daycalf"]] -  day) <= 4 * 30.5 & (cow[["daycalf"]] -  day) > 0)
   if(length(Ep) > 0){
     pregstage <- ceiling((cow[["daycalf"]][Ep] - day)/30.5)
     if(any(pregstage == 0)){print("There is a ZERO in your pregstage") ; browser()}
     cow[["energy"]][Ep] <- cow[["energy"]][Ep] + P.ENGY[pregstage]
   }
   
   
   # General culling ---------------------------------------------------------
   # general culling rates will increase with age
   
   # find healthy cows
   hc<- which(cow[["status"]] == 1) # some cows that have a mobility score >= 2 might be culled for other reasons
   parCAtcull <- cow[["par"]][hc] ; parCAtcull[parCAtcull > 5] <- 5
   gencull <- hc[rbinom(length(hc), 1, p_gencull[parCAtcull]/ts) == 1]
   # flag cows to be removed at lactaion end for general culling reasons
   if(length(gencull) > 0){
     cow[["status"]][gencull]<- 4
   }
   
   # probability of a general cull cow sucumbing to mortality
   # find general cull cows
   pmort<- which(cow[["status"]] == 4)
   mort<-  pmort[rbinom(length(pmort), 1, p_mort/ts) == 1]
   if(length(mort) > 0){
     cow[["status"]][mort] <- 5
     cow[["cull"]][mort] <- 5
     cow[["rhenter"]][mort] <- rgeom(length(mort), p_rh) + 1 + day
     
     z<- c("dim", "mdl", "yield", "yield.rdc", "rpl", "detoest", "pregnant", "status",
           "oestrus", "daycalf", "dayendvwp", "daycullfert") # variables to reset in cow list (add variables as you bring them to life in the model)
     cow[z]<- lapply(cow[z], function(x) {x[mort] <- 0; x})
     
   }
   
   gencull <- which(cow[["status"]] == 4)
   if(length(gencull) > 0){
     cow[["cull"]][gencull]<- 4
     cow[["yield"]][gencull] <- 0
     cow[["yield.rdc"]][gencull] <- 0
   }
   
   # find cows that are not pregnant after n days after first oestrus and/or failed to conceive after max inseminations
   
   # fertcull <- which(cow[["pregnant"]] == 0 & cow[["daycullfert"]] == day) # This index is used when a max daysOpen culling rule is applied
   # fertcull <- which(cow[["pregnant"]] == 0 & cow[["n_insem"]] + cow[["m_oest"]]  == max_insem & cow[["yield"]] < yield_thresh.AI)
   # fertcull <- which(cow[["pregnant"]] == 0 & cow[["n_insem"]] + cow[["m_oest"]]  == max_insem) #ok
   # fertcull <- which(cow[["pregnant"]] == 0 & cow[["n_insem"]] == max_insem & cow[["yield"]] < yield_thresh.AI)
   # fertcull <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["n_insem"]] >= max_insem & cow[["yield"]] < yield_thresh.AI & cow[["n_insem_pregloss"]] >= max_insem_pregloss)  
   
   # fertcull1 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["n_insem"]] >= max_insem)
   # fertcull2 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["n_insem_pregloss"]] >= max_insem_pregloss)
   # fertcull3 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["yield"]] < yield_thresh.AI)
   # fertcull <- c(fertcull1, fertcull2, fertcull3)
   
   fertcull1 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["n_insem"]] >= max_insem)
   fertcull2 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["n_insem_pregloss"]] >= max_insem_pregloss)
   # Ann: I fixed here at cow[["status"]] below: cows that fet culled by fertility cull (2), max parity (3)gen cull (4), mortality (5) will not be selected
   fertcull3 <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["dim"]] > 20 & cow[["yield"]] < yield_thresh.AI & !(cow[["status"]]  %in% c(2,3,4,5) ) ) # DIM > 20 is to prevent the culling rule for the early lactation cow after calving, status = 1 to prevent gen cull cow being fert cull
   fertcull <- c(fertcull1, fertcull2, fertcull3)
   
   # fertcull <- which(cow[["pregnant"]] == 0 & cow[["calf"]] == 0 & cow[["dim"]] >10 & cow[["yield"]] < yield_thresh.AI)
   
   # fertcull1 <- which(cow[["pregnant"]] == 0 & cow[["yield"]] < yield_thresh.AI)
   # fertcull2 <- which(cow[["n_insem"]] == max_insem)
   # fertcull3 <- which(cow[["n_insem_pregloss"]] == max_insem_pregloss)
   # fertcull <- c(fertcull1, fertcull2, fertcull3)
   
   #CEVA
   #if(CEVA_scen %in% 1:3){
   #fertcull <- which(cow[["pregnant"]] == 0 & cow[["n_insem"]] == max_insem)}
   # flag cow which will be culled at end of lactation for fertility reasons
   
   if(length(fertcull) > 0){
     cow[["status"]][fertcull] <- 2
     z<- c("oestrus", "daycalf", "dayendvwp") # variables to reset in lookup list
     cow[z] <-lapply(cow[z], function(x) {x[fertcull] <- 0; x})
   }
   
   
   ## remove cow at end of lactation due to fertility culling reasons and give culling reason
   fc<- which(cow[["status"]] == 2 & cow[["yield"]] - cow[["yield.rdc"]] < yield_thresh)
   # fc2 <- which(cow[["status"]] == 6 & cow[["pregnant"]] == 0 & cow[["yield"]] < yield_thresh & cow[["calf"]] == 0) # remove cows that became lame after tagged as fert cull
   # fc <- c(fc, fc2)
   if(length(fc) > 0){
     cow[["cull"]][fc]<- 2
   }
   
   # find max par cows
   mpc<- which(cow[["par"]] == max_par & cow[["yield"]] < yield_thresh)
   if(length(mpc) > 0){
     cow[["status"]][mpc] <- 3
     cow[["cull"]][mpc]   <- 3 # tag as cull for max_par reasons (=3)
     # cow[c("status", "par", "mdl", "dim", 'yield')]<- lapply(cow[c("status", "par", "mdl", "dim", 'yield')], function(x) {x[mpc] <- 0; x})
   }
   
   
   # adjust number of milking days left for cull cows if still producing above the threshold
   adjmd <- which(cow[["dim"]] == cow[["mdl"]] & cow[["pregnant"]] == 0 & cow[["yield"]] >= yield_thresh)
   if(length(adjmd) > 0){
     cow[["mdl"]][adjmd] <- cow[["mdl"]][adjmd] + 1 
   }
   
   
   
   
   # Production impacts ------------------------------------------------------
   
   #~ I have left the variables here as used to calculate the economic variables below in `Economic calculations`
   
   # live weight gain adjusted in "CALVING AND MILKING"
   
   # reduced milk yield
   mlossidx <- which(cow[["pregnant"]] == 1 & ceiling((i-cow[["oestrus"]])/7) > 5 & cow[["yield"]] > 0)    #milk yield loss after 5 wk pregnancy --- Inchaisri et al 2010
   if(length(mlossidx) > 0) {
     cow[["yield.rdc"]][mlossidx] <- mloss_1 * exp(mloss_2 * ceiling((i-cow[["oestrus"]][mlossidx])/7))
   }
   
   # actual energy
   cow[["act.energy"]] <- cow[["energy"]] - (42.4 * cow[["weight"]]^0.75 + 442 * (cow[["yield"]] * 0.337 + 0.116 * fat[EparCat]*100 + 0.06 * prot[EparCat]*100)) +
     (42.4 * cow[["weight"]]^0.75 + 442 * ((cow[["yield"]]-cow[["yield.rdc"]]) * 0.337 + 0.116 * fat[EparCat]*100 + 0.06 * prot[EparCat]*100))
   
   
   
   # Economic calculations ---------------------------------------------------
   
   # insemination done in `Production`
   # hormone application done when applied (search key: CostHormoneCeva)
   # pregnancy diagnosis costs computed at moment of diagnosis (search key: CostPregDiag)
   
   # feed
   cow[["cost.feed"]] <- cow[["act.energy"]]/1000 * cost.EkVEM
   
   # culling
   cull.eur <- which(cow[["cull"]] %in% 2:4)
   if(length(cull.eur) > 0){
     cpcat <- cow[["par"]][cull.eur] ; cpcat[cpcat > use_par] <- use_par 
     x.dim <- cow[["dim"]][cull.eur]/cow[["mdl"]][cull.eur] ; x.dim[x.dim > 1] <- 1
     cow[["cost.cull"]][cull.eur] <- ((mean(rpert(length(cull.eur), cost.rear[1], cost.rear[2], cost.rear[3])) - 
                                         (cow[["weight"]][cull.eur]*perc.dress*mean(rpert(length(cull.eur), price.kg[1], price.kg[2], price.kg[3]))))/use_par) *
       (use_par - (cpcat-1 + 1*x.dim))
   }
   
   # mortality costs
   cullM.eur <- which(cow[["cull"]] == 5)
   if(length(cullM.eur) > 0){
     cpcatM <- cow[["par"]][cullM.eur] ; cpcatM[cpcatM > use_par] <- use_par 
     x.dimM <- cow[["dim"]][cullM.eur]/cow[["mdl"]][cullM.eur] ; x.dimM[x.dimM > 1] <- 1
     cow[["cost.cull.mort"]][cullM.eur] <- ((mean(rpert(length(cullM.eur), cost.rear[1], cost.rear[2], cost.rear[3])) +
                                               (cow[["weight"]][cullM.eur]*perc.dress*mean(rpert(length(cullM.eur), price.kg[1], price.kg[2], price.kg[3]))))/use_par) * 
       (use_par - (cpcatM-1 + 1*x.dimM)) + RemCost     
   }
   
   #~ I have kept this placeholder for `some culling reason` culling costs
   #~ it relates to the culling of lame cows (inactive code) in `Production`
   cullBVD.euro <- which(cow[["cull"]] == 6)
   if(length(cullBVD.euro) > 0){
     cpcat <- cow[["par"]][cullBVD.eur] ; cpcat[cpcat > use_par] <- use_par 
     x.dim <- cow[["dim"]][cullBVD.eur]/cow[["mdl"]][cullBVD.eur] ; x.dim[x.dim > 1] <- 1
     cow[["cost.cull"]][cullBVD.eur] <- ((mean(rpert(length(cullBVD.eur), cost.rear[1], cost.rear[2], cost.rear[3])) - 
                                            (cow[["weight"]][cullBVD.eur]*perc.dress*mean(rpert(length(cullBVD.eur), price.kg[1], price.kg[2], price.kg[3]))))/use_par) *
       (use_par - (cpcat-1 + 1*x.dim))
   }
   
   
   # cost milk
   cow[["cost.milk"]] <- cow[["yield.rdc"]] * MilkPrice
   
   
   # revenue milk
   milk.rev <- which(cow[["disc.milk"]] == 0)
   if(length(milk.rev) > 0){
     cow[["rev.milk"]][milk.rev] <- cow[["yield"]][milk.rev] * MilkPrice
   }
   # no revenue for discarding milk
   milk.rev2 <- which(cow[["disc.milk"]] != 0)
   if(length(milk.rev2) > 0){
     cow[["rev.milk"]][milk.rev2] <- 0
   }
   
   #DoubleOvsync related cost
   if(CEVA_scen %in% 1:2){
     dblOv.cost <- which(cow[["dayendvwp"]] - day == t_dblOvSynch)
     if(length(dblOv.cost) > 0){
       cow[["cost.hormone"]][dblOv.cost] <- cost.hormone[1]
       cow[["cost.labour.hu"]][dblOv.cost] <- t_labDoubleOv * cost_lab/60 
     }
   }
     
# Data collection ---------------------------------------------------------
   
   # collect data for each day
   cowsList[[i]] <- cow
 } # :: end of For loop ::
 # return list of daily data
 cowsList
} # :: end of model ::

# Model replication functions ---------------------------------------------

limS <- 5     # simulation limits for RAM output 
clb <- TRUE   # calibration



MySim2<- function(nFrames = xPRM$n_sim, limS = NA, clb = NA, baseline_folder) {

 if(xPRM$n_sim > limS){
  setwd(xPRM$storeWD)
  setwd("Outputs")
  if(!clb){
   setwd("Final")
   t <- format(Sys.time(), "%F %H-%M") ; t <- gsub(" ", "_", t)
   t <- paste0(t, "_", "Scenario", 
               ifelse(xPRM$CEVA_scen == 0, paste0(xPRM$CEVA_scen, xPRM$baseline_int), 
                      xPRM$CEVA_scen))
   dir.create(file.path(getwd(), t))
   setwd(dir()[dir() == t])
   RAW.dir <- getwd()
  } else{
   setwd("Calibration")
   t <- format(Sys.time(), "%F %H-%M") ; t <- gsub(" ", "_", t)
   t <- paste0(t, "_", "Scenario", 
               ifelse(xPRM$CEVA_scen == 0, paste0(xPRM$CEVA_scen, xPRM$baseline_int), 
                      xPRM$CEVA_scen))
   dir.create(file.path(getwd(), t))
   setwd(dir()[dir() == t])
  }
 } else{
  dl <- vector(mode = "list", length = as.numeric(nFrames))
 }
 # browser()
 #  Read in initialisation data
initFiles <- list.files(paste0("D:/New_cow_simulation/Outputs/Initialisation/", 
                               baseline_folder),
                        full.names = T)[1:xPRM$n_sim]
initData <- lapply(initFiles, fread)
 
 set.seed(xPRM$seed)
 # fwrite(as.data.frame(.Random.seed), "seed.csv")
 t. <- format(Sys.time(), "%F %H-%M") ; t. <- gsub(" ", "_", t.)
 save(xPRM, file = paste0("default_Parameters_", t., ".RData"))
 for(i in seq.int(1,nFrames,1)) {
  initData_I <- initData[[i]]
  if(xPRM$n_sim > limS){
   frame <- rbindlist(mkFrames(X = xPRM, initData_i = initData_I))
   frame$.id <- i
   fwrite(frame, paste0("MySimCows_", str_pad(i, 5, pad = "0"), ".csv"))
   # rm(frame)
  }
  else{
   print(i)
   dl[[i]]<- rbindlist(mkFrames())
  }
 }
 
 if(xPRM$n_sim <= limS){
  rbindlist(dl, idcol = TRUE)
 }
}

