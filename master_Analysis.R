setwd("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/GIT_CEVA")
# set directory  to scripts directory
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
library(ggplot2)


# Source inputs and set time horizon and number of replications -----------------------------------------------------------------------

xPRM         <- mget(load("reprodAction_defaultParameters.RData", 
                          envir=(NE. <- new.env())), envir=NE.)$xPRM; rm(NE.)  # input parameters in list

# control herd size, number of simulations and time horizon here
xPRM$n_cows    <- 200      # number of simulated cows per time step
xPRM$n_sim     <- 2    # number of simulations/replications
xPRM$n_yrs     <- 12    # Number of simulation years

daysOpen.mat <- fread("daysOpen_mat.csv")                                      # related to fertility culling decisions
# ensure daysOpen.mat is set to correct herd size
if(xPRM$n_cows != nrow(daysOpen.mat)){
 print("The number of cows you want to simulate is not equal to the cows used to generate `daysOpen.mat`. 
        Please, ensure the the number of cows in `daysOpen.mat` is equal to the number of cows you want to simulate. 
        Your `daysOpen.mat` will be removed from the environment now!")
 rm(daysOpen.mat)
}

# Source model and other functions ----------------------------------------------------------------------------------------------------
if(xPRM$CEVA_scen == 0){
 source("master_ProdMod_baseline.R")
}else{
 source("master_ProdMod_scenarios.R")
}

source("ProdMod_internalFunc.R")  


# Run model ---------------------------------------------------------------------------------------------------------------------------
# run n_sim replications and store locally 
if(xPRM$CEVA_scen == 0){
  ptm <- proc.time()
  MySim2(nFrames = xPRM$n_sim, limS = 0, clb = T)
  proc.time() - ptm #~ model run time
}else{
ptm <- proc.time()
MySim2(nFrames = xPRM$n_sim, limS = 0, clb = T, baseline_folder = "2024-08-20_10-17_Scenario0TRUE")  #from the init data (baseline)change later
proc.time() - ptm #~ model run time
}

# Burn-in evaluation ------------------------------------------------------------------------------------------------------------------

burn_demographics <- rbindlist(
 lapply(list.files(pattern = ".csv"), function(j){
 print(j)
 k <- fread(j) %>%
  # mutate(par = if_else(par > 5, 5, par)) %>%
  group_by(.id, space, UID, par, year) %>%
  summarise(N = n()) %>%
  group_by(.id, space, year) %>%
  filter(max(N) == N) %>%
  group_by(.id, par, year) %>%
  summarise(share = n()/xPRM$n_cows) %>%
  group_by(par, year) %>%
  summarise(share = mean(share))
}),
idcol = T
)

burn_demographics %>%
 group_by(year, par) %>%
 summarise(distribution = mean(share)) %>%
 ggplot() +
 geom_line(aes(x = year, y = distribution, colour = factor(par)), size = 1.2) +
 geom_vline(xintercept = 8)

burn_disorderprev <- rbindlist(
 lapply(list.files(pattern = ".csv"), function(j){
  print(j)
  k <- fread(j) %>%
   # mutate(par = if_else(par > 5, 5, par)) %>%
   group_by(UID, par) %>%
   mutate(incAn = if_else(lag(an_cyc1 == 0) & an_cyc1 == 1, 1, 0), 
          incCod = if_else(lag(cod == 0) & cod == 1, 1, 0),
          PPan = if_else(dim <= 86 & dim >= 65, 1, 0),
          PPcod = if_else(dim <= 105 & dim >= 65 & treated == 0 & 
                           an_cyc1 == 0, 1, 0)) %>%
   filter(detoest == 1 | ((oestrus - 21) == i & m_oest != 0)) %>%
   mutate(normalCyc = if_else(an_cyc1 == 0 & cod == 0, 1, 0)) %>%
   select(-(rhenter:rev.milk)) %>%
   group_by(year) %>%
   summarise_at(.vars = c("incAn", "incCod", "PPan", "PPcod", "suboest", "normalCyc", "detoest"), .funs = sum, na.rm = TRUE) %>%
   mutate(incAnR = incAn/PPan,
          incCodR = incCod/PPcod,
          incSub = suboest/normalCyc,
          detRate = detoest/normalCyc)
 }),
 idcol = T
)

burn_disorderprev %>%
 select(.id:year, incAnR:detRate) %>%
 pivot_longer(cols = incAnR:detRate, names_to = "OutputVariable", values_to = "Rates") %>%
 group_by(year, OutputVariable) %>%
 summarise(mu = mean(Rates),
           sd = sd(Rates)) %>%
ggplot() + 
 geom_point(aes(x = year, y = mu)) +
  geom_errorbar(aes(x = year, y = mu, ymax = mu + sd, ymin = mu - sd)) +
 facet_grid(cols = vars(OutputVariable), scales = "free")

dev.off() #use this if ggplot is not working and then run again

# Total culls
total_cull <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      select(year, cull) %>%
      filter(cull != 0) %>%
      summarise(nCulls = n())
  }),
  idcol = T
)

# Total milk yield
total_yield <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
  print(j)
  k <- fread(j) %>%
    #select(yield, yield.rdc) %>%
    summarise(gross.milk = sum(yield),
              loss.milk  = sum (yield.rdc),
              net.milk   = sum(gross.milk) - sum(loss.milk)) 
  }),
idcol = T
)


# Model convergence evaluation --------------------------------------------------------------------------------------------------------
converg_incidence0 <- burn_disorderprev %>%
 group_by(.id) %>%
 summarise_at(.vars = c("incAn", "incCod", "suboest", "detoest"), .funs = sum)


converg_incidence1 <- apply(converg_incidence0[, -1], 2, function(j) {
 sapply(1:nrow(converg_incidence0), function(k){
  var(j[1:k])
 })
})

par(mfrow = c(1, 4))
apply(converg_incidence1, 2, function(j) {plot(j, type = "l", xlab = "nth replication", ylab = "variance")})
# mapply(function(converg_incidence1, title){
#  plot(converg_incidence1, type = "l", main = title, xlab = "nth replication", ylab = "variance")
# }, converg_incidence1, names(converg_incidence1))

# plot variance of total culls
plot(sapply(2:nrow(total_cull), function(x) var(total_cull$nCulls[1:x])), type = "l")

# plot variance of total milk yield
plot(sapply(2:nrow(total_yield), function(x) var(total_yield$net.milk[1:x])), type = "l")



tmp <- matrix(NA, ncol = 20, nrow = 50)
for(i in seq_along(unique(burn_disorderprev$.id))) {
 for(j in seq_along(unique(burn_disorderprev$year))){
  tmp[i,j] <- var(burn_disorderprev$incAn[burn_disorderprev$.id %in% 1:i & burn_disorderprev$year  == j])
 }
}
par(mfrow = c(4, 5)) ; apply(tmp[, 1:20], 2, function(j) {plot(j, type = "l", xlab = "nth replication", ylab = "variance")})


# Reproductive indicators -------------------------------------------------------------------------------------------------------------

# Calving interval; calving to first AI; calving to conception (days open)
# choose which scenario
repro_indicators3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) 
    l0 <- k %>%
      filter(calf == 1, prevdaycalf != 0) %>%
      mutate(variable = "calving interval", 
             value = daycalf - prevdaycalf) %>%
      select(year, variable, value, par)
    l1 <- k %>% 
      filter(detoest == 1, n_insem == 1) %>%
      mutate(variable = "calving to 1st AI",
             value = dim) %>%
      select(year, variable, value, par)
    l2 <- k %>%
      filter(detoest == 1, pregnant == 1) %>%
      mutate(variable1 = "calving to conception",
             variable2 = "n inseminations to conception", # should be conception instead of calving
             value1    = dim + 30,
             value2    = n_insem) %>%
      select(year, variable1, variable2, value1, value2, par) %>%
      pivot_longer(cols = variable1:value2,
                   names_to = c(".value", "tmp"), 
                   names_pattern = "([^\\.]*)\\.*(\\d{1})",
                   names_repair = "minimal") %>%
      select(-tmp)

    l <- rbind(l0, l1, l2)
    return(l)  
  }),
  idcol = T
)

# at cow level
repro_indicators0$scenario = "baseline"
repro_indicators1$scenario = "1"
repro_indicators2$scenario = "2"
repro_indicators3$scenario = "3"

#combine (select which scenario)
repro_indicators <- rbind(repro_indicators0, repro_indicators1) %>% filter(year %in% 7:9)
repro_indicators_all <- rbind(repro_indicators0, repro_indicators1, repro_indicators2, repro_indicators3) %>% filter(year %in% 7:9)

ggplot(repro_indicators_all, aes(x = as.factor(year), y = value)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(variable), scales = "free_y")
# facet_grid(rows = vars(scenario), cols = vars(variable), scales = "free_y")

#mean of each variable
tapply(repro_indicators_all$value, 
       list(repro_indicators_all$variable, repro_indicators_all$year, repro_indicators_all$scenario), 
       mean)

#quantile of each variable
tapply(repro_indicators_all$value, 
       list(repro_indicators_all$variable, repro_indicators_all$year, repro_indicators_all$scenario), 
       quantile, p=0.95)

ggplot(repro_indicators_all, aes(x = as.factor(year), y = value)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(variable), scales = "free_y")

# at herd-level

repro_indicators_all_herd <- repro_indicators_all %>%
  group_by(scenario, .id, year, variable) %>%
  summarise(output = mean(value))

# an example of one output variable for a chosen year:
summary(repro_indicators_all_herd$output[repro_indicators_all_herd$year == 7 & repro_indicators_all_herd$variable == "calving interval" & repro_indicators_all_herd$scenario == "baseline"])
quantile(repro_indicators_all_herd$output[repro_indicators_all_herd$year == 7 & repro_indicators_all_herd$variable == "calving interval" & repro_indicators_all_herd$scenario == "baseline"], p=0.95)
quantile(repro_indicators_all_herd$output[repro_indicators_all_herd$year == 9 & repro_indicators_all_herd$variable == "calving interval" & repro_indicators_all_herd$scenario == "1"])

ggplot(repro_indicators_all_herd, aes(x = as.factor(year), y = output)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(variable), scales = "free_y")


## daily average cows in milk
MY <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
  filter(yield != 0) %>%
  group_by(.id, year, i) %>%
  summarise(mu = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(mu))
  }),
    idcol = T
  )

## absolute culling numbers
cull3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      filter(cull != 0) %>%
      group_by(cull, year) %>%
      summarise(mu = n()) #%>%
      # group_by(year) %>%
      # summarise(mu = mean(mu))
  }),
  idcol = T
)

cull0$scenario = "baseline"
cull1$scenario = "1"
cull2$scenario = "2"
cull3$scenario = "3"

#combine (select which scenario)
cull <- rbind(cull0, cull1) %>% filter(year %in% 7:17)
cull_all <- rbind(cull0, cull1, cull2, cull3) %>% filter(year %in% 7:9)

ggplot(cull_all_total, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

# total culled per culling reason
ggplot(cull_all, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(cull))

#total culled overall
ggplot(cull_all %>% 
         group_by(.id, year, scenario) %>%
         mutate(mu = sum(mu)), aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

cull_all_total <- cull_all %>% 
                  group_by(.id, year, scenario) %>%
                  summarise(mu = sum(mu))

tapply(cull_all_total$mu, list(cull_all_total$year, cull_all_total$scenario), mean)
tapply(cull_all_total$mu, list(cull_all_total$year, cull_all_total$scenario), quantile, p=0.95)


# absolute culling numbers (original from cullfert)
cullfert0 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      filter(cull != 0) %>%
      group_by(cull, year) %>%
      summarise(mu = n()) #%>%
    # group_by(year) %>%
    # summarise(mu = mean(mu))
  }),
  idcol = T
)

cullfert0$scenario = "baseline"
cullfert1$scenario = "1"
cullfert2$scenario = "2"
cullfert3$scenario = "3"

#combine (select which scenario)
cullfert <- rbind(cullfert0, cullfert1) %>% filter(year %in% 7:17)
cullfert_all <- rbind(cullfert0, cullfert1, cullfert2, cullfert3) %>% filter(year %in% 7:9)

ggplot(cullfert_all, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

# total culled per culling reason
ggplot(cullfert_all, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(cull))

#total culled overall
ggplot(cullfert_all %>% 
         group_by(.id, year, scenario) %>%
         mutate(mu = sum(mu)), aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))


tapply(cullfert_all$mu, list(cullfert_all$year, cullfert_all$scenario), mean)
tapply(cullfert_all$mu, list(cullfert_all$year, cullfert_all$scenario), quantile, p=0.95)

## number of pregnancies
npreg0 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      filter(detoest == 1, pregnant == 1) %>%
      group_by(.id, year) %>%
      summarise(n = n()) %>%
      group_by(year) %>%
      summarise(mu = mean(n))
  }),
  idcol = T
)

npreg0$scenario = "baseline"
npreg1$scenario = "1"
npreg2$scenario = "2"
npreg3$scenario = "3"

npreg_all <- rbind(npreg0, npreg1, npreg2, npreg3) %>% filter(year %in% 7:9)

ggplot(npreg_all, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

tapply(npreg_all$mu, list(npreg_all$year, npreg_all$scenario), mean)
tapply(npreg_all$mu, list(npreg_all$year, npreg_all$scenario), quantile, p=0.95)

## number of pregnancies per parity
npreg2_par <- rbindlist(
      lapply(list.files(pattern = ".csv"), function(j){
      print(j)
      k <- fread(j) %>%
  filter(detoest == 1, pregnant == 1) %>%
  group_by(.id, year, par) %>%
  summarise(n = n()) %>%
  group_by(year, par) %>%
  summarise(mu = mean(n))
    }),
  idcol = T
  )

npreg0_par$scenario = "baseline"
npreg1_par$scenario = "1"
npreg2_par$scenario = "2"
npreg3_par$scenario = "3"

npreg_par <- rbind(npreg0_par, npreg1_par, npreg2_par, npreg3_par) %>% filter(year %in% 7:17)
npreg_all_6 <- rbind(npreg0, npreg1_6, npreg2_6, npreg3_6) %>% filter(year %in% 7:17)

ggplot(npreg_par, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(par), scales = "free_y")

## number of calf born
ncalf3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      filter(calf == 1 & par != 1) %>%
      group_by(year) %>%
      summarise(n = n()) %>%
      group_by(year) %>%
      summarise(mu = mean(n))
  }),
  idcol = T
)

ncalf0$scenario = "baseline"
ncalf1$scenario = "1"
ncalf2$scenario = "2"
ncalf3$scenario = "3"

ncalf_all <- rbind(ncalf0, ncalf1, ncalf2, ncalf3) %>% filter(year %in% 7:9)

ggplot(ncalf_all, aes(x = as.factor(year), y = mu)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

tapply(ncalf_all$mu, list(ncalf_all$year, ncalf_all$scenario), mean)
tapply(ncalf_all$mu, list(ncalf_all$year, ncalf_all$scenario), quantile, p=0.05)

# at herd-level

ncalf_all_herd <- ncalf_all %>%
  group_by(scenario, .id, year) %>%
  summarise(output = mean(mu))

# an example of one output variable for a chosen year:
summary(ncalf_all_herd$output[ncalf_all_herd$year == 9 & ncalf_all_herd$scenario == "1"])
quantilesummary(ncalf_all_herd$output[ncalf_all_herd$year == 9 & ncalf_all_herd$scenario == "1"])

ggplot(ncalf_all_herd, aes(x = as.factor(year), y = output)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

# Other indicators --------------------------------------------------------

##Parity distribution
par3 <- rbindlist(
    lapply(list.files(pattern = ".csv"), function(j){
      print(j)
      k <- fread(j) %>%
        # mutate(par = if_else(par > 5, 5, par)) %>%
        group_by(.id, space, UID, par, year) %>%
        summarise(N = n()) %>%
        group_by(.id, space, year) %>%
        filter(max(N) == N) %>%
        group_by(.id, par, year) %>%
        summarise(share = n()/xPRM$n_cows) %>%
        group_by(par, year) %>%
        summarise(share = mean(share))
    }),
    idcol = T
  )

par0$scenario = "baseline"
par1$scenario = "1"
par2$scenario = "2"
par3$scenario = "3"

par_all <- rbind(par0, par1, par2, par3) %>% filter(year %in% 7:17)

par_all %>%
  group_by(year, par, scenario) %>%
  summarise(distribution = mean(share)) %>%
  ggplot() +
  geom_line(aes(x = year, y = distribution, colour = factor(scenario)), size = 1.2) +
  geom_vline(xintercept = 8) +
  facet_wrap(facets = vars(par), scales = "free_y")


# averaged total milk yield per year 
avemilk0_3 <- rbindlist(
    lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
    group_by(.id, year) %>%
    summarise(gross.milk = sum(yield),
            loss.milk  = sum (yield.rdc),
            net.milk   = sum(gross.milk) - sum(loss.milk)) 
    }),
  idcol = T
  )

# avemilk1_3 <- avemilk0_3 %>%
#   ungroup() %>%
#   select(-.id) %>%
#   group_by(year) %>%
#   summarise_all(.funs = list(mu = ~mean(.),
#                              q05 = ~quantile(., p =0.05),
#                              q95 = ~quantile(., p = 0.95)))

# avemilk1_0$scenario = "baseline"
# avemilk1_1$scenario = "1"
# avemilk1_2$scenario = "2"
# avemilk1_3$scenario = "3"

avemilk0_0$scenario = "baseline"
avemilk0_1$scenario = "1"
avemilk0_2$scenario = "2"
avemilk0_3$scenario = "3"

avemilk0_all <- rbind(avemilk0_0, avemilk0_1, avemilk0_2, avemilk0_3) %>% 
                select(-.id) %>%
                filter(year %in% 7:9)

ggplot(avemilk0_all, aes(x = as.factor(year), y = net.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

ggplot(avemilk0_all, aes(x = as.factor(year), y = loss.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

ggplot(avemilk0_all, aes(x = as.factor(year), y = gross.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

tapply(avemilk0_all$net.milk, list(avemilk0_all$year, avemilk0_all$scenario), mean)
tapply(avemilk0_all$net.milk, list(avemilk0_all$year, avemilk0_all$scenario), quantile, p=0.95)
tapply(avemilk0_all$loss.milk, list(avemilk0_all$year, avemilk0_all$scenario), quantile, p=0.95)
tapply(avemilk0_all$gross.milk, list(avemilk0_all$year, avemilk0_all$scenario), quantile, p=0.95)


# averaged total milk yield per year per PARITY
avemilk3_par <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      group_by(.id, year, par) %>%
      summarise(gross.milk = sum(yield),
                loss.milk  = sum (yield.rdc),
                net.milk   = sum(gross.milk) - sum(loss.milk)) 
  }),
  idcol = T
)

# avemilk1_3 <- avemilk0_3 %>%
#   ungroup() %>%
#   select(-.id) %>%
#   group_by(year) %>%
#   summarise_all(.funs = list(mu = ~mean(.),
#                              q05 = ~quantile(., p =0.05),
#                              q95 = ~quantile(., p = 0.95)))

# avemilk1_0$scenario = "baseline"
# avemilk1_1$scenario = "1"
# avemilk1_2$scenario = "2"
# avemilk1_3$scenario = "3"

avemilk0_par$scenario = "baseline"
avemilk1_par$scenario = "1"
avemilk2_par$scenario = "2"
avemilk3_par$scenario = "3"

avemilk_all_par <- rbind(avemilk0_par, avemilk1_par, avemilk2_par, avemilk3_par) %>% 
  select(-.id) %>%
  filter(year %in% 7:15)

ggplot(avemilk_all_par, aes(x = as.factor(year), y = net.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

ggplot(avemilk_all_par, aes(x = as.factor(year), y = net.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(par), scales = "free_y")

tapply(avemilk_all_par$net.milk, list(avemilk_all_par$year, avemilk_all_par$scenario, avemilk_all_par$par), mean)


#average milk production on 305d
mean305_0 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      filter(day == 305)  %>%
      group_by(.id, year) %>%
      summarise(gross.milk = sum(yield),
                loss.milk  = sum (yield.rdc),
                net.milk   = sum(gross.milk) - sum(loss.milk)) 
  }),
  idcol = T
  )

summary(mean305_0$net.milk)
quantile(mean305_0$net.milk, p=0.95)


# number of protocol applications
nprotocol3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      filter(cost.hormone != 0 | cost.hormone.an != 0 | 
             cost.hormone.cod != 0 | cost.hormone.hi != 0)  %>%
      group_by(.id, year, cost.hormone, cost.hormone.an, cost.hormone.cod, cost.hormone.hi) %>%
      summarise(N = n())
  }),
  idcol = T
)

nprotocol0$scenario = "baseline"
nprotocol1$scenario = "1"
nprotocol2$scenario = "2"
nprotocol3$scenario = "3"

#for every type of protocol
nprotocol_all <- rbind(nprotocol0, nprotocol1, nprotocol2, nprotocol3) %>% 
  select(-.id) %>%
  filter(year %in% 7:9)

nprotocol_all <- nprotocol_all %>% mutate(total.hormone.app = cost.hormone + cost.hormone.an + cost.hormone.cod
                                            + cost.hormone.hi)

tapply(nprotocol_all$N, list(nprotocol_all$year, nprotocol_all$scenario,  nprotocol_all$total.hormone.app), mean)
tapply(nprotocol_all$N, list(nprotocol_all$year, nprotocol_all$scenario,  nprotocol_all$cost.hormone), mean)
tapply(nprotocol_all$N, list(nprotocol_all$year, nprotocol_all$scenario,  nprotocol_all$cost.hormone.an), mean)
tapply(nprotocol_all$N, list(nprotocol_all$year, nprotocol_all$scenario,  nprotocol_all$cost.hormone.cod), mean)
tapply(nprotocol_all$N, list(nprotocol_all$year, nprotocol_all$scenario,  nprotocol_all$cost.hormone.hi), mean)

ggplot(nprotocol_all, aes(x = as.factor(year), y = N)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(total.hormone.app), scales = "free_y")

#for the total protocol

nprotocol_all_total <- rbind(nprotocol0, nprotocol1, nprotocol2, nprotocol3) %>% 
  select(-1) %>%
  filter(year %in% 7:9)

nprotocol_all_total <- nprotocol_all_total %>%
                       group_by(.id, year, scenario) %>%
                       summarise(total.app = sum(N))

ggplot(nprotocol_all_total, aes(x = as.factor(year), y = total.app)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))
  

tapply(nprotocol_all_total$total.app, list(nprotocol_all_total$year, nprotocol_all_total$scenario), mean)
tapply(nprotocol_all_total$total.app, list(nprotocol_all_total$year, nprotocol_all_total$scenario), quantile, p=0.95)

# number of labour time in minute
nminutelab3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      select(year, cost.labour.ai, cost.labour.hu) %>%
            filter(cost.labour.ai != 0 | cost.labour.hu != 0)  %>%
            pivot_longer(cols = c("cost.labour.ai", "cost.labour.hu")) %>%
            group_by(year, name) %>%
      filter(value !=0 ) %>%
      group_by(year, name, value) %>%
      summarise(N_applications = n()) %>%
      mutate(total.minutes = value/(xPRM$cost_lab/60) * N_applications)
  }),
  idcol = T
)

nminutelab0$scenario = "baseline"
nminutelab1$scenario = "1"
nminutelab2$scenario = "2"
nminutelab3$scenario = "3"

nminutelab_all <- rbind(nminutelab0, nminutelab1, nminutelab2, nminutelab3) %>% filter(year %in% 7:9)

tapply(nminutelab_all$total.minutes, list(nminutelab_all$year, nminutelab_all$scenario,  nminutelab_all$name), mean)
tapply(nminutelab_all$total.minutes, list(nminutelab_all$year, nminutelab_all$scenario,  nminutelab_all$name), quantile, p=0.95)

ggplot(nminutelab_all, aes(x = as.factor(year), y = total.minutes)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(name), scales = "free_y")
  
# Economic outputs --------------------------------------------------------

# Total economic output per year

MySimCows_0 <- fread("C:/Users/ardilasunu/OneDrive/CEVA/Outputs/Calibration/2022-12-22_19-26_Scenario0TRUE/MySimCows_00001.csv")
All_Vars <- colnames(MySimCows_0)

EcoSummary3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
  group_by(.id, year) %>%
  select(rev.milk, rev.calf, grep(pattern = "cost", All_Vars, value = T)) %>%
  summarise_all(sum) %>%
  mutate(total.cost = cost.insem + cost.feed + cost.cull+ cost.milk +
           cost.milk.disc + cost.PD + cost.hormone + cost.hormone.an +
           cost.hormone.cod + cost.hormone.hi + cost.labour.ai + cost.labour.hu + cost.calf, 
         NER = rev.milk + rev.calf - total.cost)
  }),
  idcol = T
  )

EcoSummary0$scenario = "baseline"
EcoSummary1$scenario = "1"
EcoSummary2$scenario = "2"
EcoSummary3$scenario = "3"

EcoSummary_all <- rbind(EcoSummary0, EcoSummary1, EcoSummary2, EcoSummary3) %>% 
  select(-.id) %>%
  filter(year %in% 7:9)

#get total cost hormone
EcoSummary_all <- EcoSummary_all %>% mutate(total.cost.hormone = cost.hormone + cost.hormone.an + cost.hormone.cod
                                            + cost.hormone.hi)

EcoSummary_all <- EcoSummary_all %>% mutate(total.cost.labour = cost.labour.ai + cost.labour.hu)

EcoSummary_all <- EcoSummary_all %>% mutate(rev.milk.net = rev.milk - cost.milk) #net milk yield

EcoSummary_all <- EcoSummary_all %>% mutate(total.cost.1 = total.cost - cost.milk) #reduced by cost.milk, no longer use at manuscript


ggplot(EcoSummary_all, aes(x = as.factor(year), y = total.cost.hormone)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

ggplot(EcoSummary_all, aes(x = as.factor(year), y = NER)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single"))

tapply(EcoSummary_all$NER, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$rev.milk, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$rev.calf, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.05)
tapply(EcoSummary_all$cost.insem, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$cost.feed, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$cost.cull, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$cost.milk, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$cost.hormone, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$cost.hormone.an, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$cost.hormone.cod, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$cost.hormone.hi, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$total.cost.hormone, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$cost.PD, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$cost.labour.ai, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$cost.labour.hu, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$total.cost.labour, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$cost.calf, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$total.cost, list(EcoSummary_all$year, EcoSummary_all$scenario), mean)
tapply(EcoSummary_all$total.cost.1, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)
tapply(EcoSummary_all$rev.milk.net, list(EcoSummary_all$year, EcoSummary_all$scenario), quantile, p=0.95)


#Economic output per PARITY per year
EcoSummary_par3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
    print(j)
    k <- fread(j) %>%
      group_by(.id, year, par) %>%
      select(rev.milk, grep(pattern = "cost", All_Vars, value = T)) %>%
      summarise_all(sum) %>%
      mutate(total.cost = cost.insem + cost.feed + cost.cull+ cost.milk +
               cost.milk.disc + cost.PD + cost.hormone + cost.hormone.an +
               cost.hormone.cod + cost.hormone.hi + cost.labour + cost.calf, 
             NER = rev.milk - total.cost)
  }),
  idcol = T
)

EcoSummary_par0$scenario = "baseline"
EcoSummary_par1$scenario = "1"
EcoSummary_par2$scenario = "2"
EcoSummary_par3$scenario = "3"

EcoSummary_par_all <- rbind(EcoSummary_par0, EcoSummary_par1, EcoSummary_par2, EcoSummary_par3) %>% 
  select(-.id) %>%
  filter(year %in% 7:9)

ggplot(EcoSummary_par_all, aes(x = as.factor(year), y = NER)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(par), scales = "free_y")



#explore economic var at COW level per PARITY
EcoSum_cowpar3 <- rbindlist(
  lapply(list.files(pattern = ".csv"), function(j){
      print(j)
      k <- fread(j) %>%
  group_by(.id, UID, year, par) %>%
  select(rev.milk, grep(pattern = "cost", All_Vars, value = T)) %>%
  summarise_all(sum) %>%
  mutate(total.cost = cost.insem + cost.feed + cost.cull+ cost.milk +
           cost.milk.disc + cost.PD + cost.hormone + cost.hormone.an +
           cost.hormone.cod + cost.hormone.hi + cost.labour + cost.calf, 
         NER = rev.milk - total.cost) %>%
  group_by(year, par) %>%
  summarise(rev.milk = mean(rev.milk), total.cost = mean(total.cost), NER = mean (NER))
  }),
  idcol = T
)

EcoSum_cowpar0$scenario = "baseline"
EcoSum_cowpar1$scenario = "1"
EcoSum_cowpar2$scenario = "2"
EcoSum_cowpar3$scenario = "3"

EcoSum_cowpar_all <- rbind(EcoSum_cowpar0, EcoSum_cowpar1, EcoSum_cowpar2, EcoSum_cowpar3) %>% 
  select(-.id) %>%
  filter(year %in% 7:9)

ggplot(EcoSum_cowpar_all, aes(x = as.factor(year), y = rev.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(par), scales = "free_y")


#ggplot in the cow level (not grouping by parity)
ggplot(EcoSum_cowpar_all, aes(x = as.factor(year), y = rev.milk)) + 
  geom_boxplot(aes(group = interaction(year, scenario), fill = as.factor(scenario)), position = position_dodge(preserve = "single")) # +
  # facet_wrap(facets = vars(par), scales = "free_y")
