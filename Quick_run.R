setwd("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/Cow simulation")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Source inputs -----------------------------------------------------------


source("ProdMod_internalFunc.R")  
daysOpen.mat <- fread("daysOpen_mat.csv")  
xPRM         <- mget(load("reprodAction_defaultParameters.RData", 
                          envir=(NE. <- new.env())), envir=NE.)$xPRM; rm(NE.)  # input parameters in list
# related to fertility culling decisions


# control herd size, number of simulations and time horizon here during preliminary runs
xPRM$n_cows <- 200      # number of simulated cows per time step
xPRM$n_sim  <- 2      # number of simulations
xPRM$n_yrs  <- 12

# *NB* set scenario in default script

# run one simulation and store in RAM
set.seed(1)
ptm <- proc.time()
MySim_cows <- rbindlist(mkFrames(X = xPRM))
proc.time() - ptm #~ time to run model for one simulation
rm(MySim_cows)
ptm <- proc.time()
MySim_cows <- MySim2(nFrames = xPRM$n_sim, limS = 25, clb = T)
proc.time() - ptm #~ time to run model for one simulation

# run n_sim simulations
rm(MySim_cows)
ptm <- proc.time()
MySim_cows <- MySim2(nFrames = xPRM$n_sim, limS = 24, clb = T,
                     baseline_folder = "2022-05-22_17-32_Scenario0TRUE")
proc.time() - ptm #~ time to run model for one simulation



# Model basic insights -----------------------------------------------------------------------------------------------------------------


#~ mean lactation curves
MySim_cows %>%
  mutate(par = ifelse(par > 3, ">3", par),
         par = factor(par, levels = c(1, 2, 3, ">3"))) %>%
  group_by(par, dim, year) %>%
  summarise(mu = mean(yield), sigma = sd(yield)) %>%
  ggplot(aes(x = dim, y = mu)) +
  geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma, fill = par, alpha = 0.01)) +
  geom_line(aes(colour = par), size = 0.8) +
  facet_grid(cols = vars(par), rows = vars(year))

#~ herd demographic - parity distribution
demo <- MySim_cows %>%
  # mutate(par = if_else(par > 5, 5, par)) %>%
  group_by(.id, space, UID, par, year) %>%
  summarise(N = n()) %>%
  group_by(.id, space, year) %>%
  filter(max(N) == N) %>%
  group_by(.id, par, year) %>%
  summarise(share = n()/xPRM$n_cows) %>%
  group_by(par, year) %>%
  summarise(share = mean(share))
ggplot(demo, aes(x = year, y = share, label=round(share, 2))) + 
  geom_line(aes(colour = factor(par)), size = 0.9) +
  geom_text(aes(label=round(share, 2)),hjust=0, vjust=0)


Culls <- MySim_cows %>%
  filter(cull != 0) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n),
            sigma = sd(n))
ggplot(Culls, aes(x = year, y = mu, fill = factor(par))) +
  geom_col() +
  facet_grid(cols = vars(cull))


CalvInt <- MySim_cows %>%
  filter(year != 1, calf == 1, prevdaycalf != 0) %>%
  mutate(interval = daycalf - prevdaycalf) 

ci0 <- CalvInt$interval
ci1 <- CalvInt$interval

summary(ci0) ; summary(ci1)


# Heat detection rate output ----------------------------------------------------------------------------------------------------------

tmp = MySim_cows %>% filter(detoest == 1 | ((oestrus - 21) == i & m_oest != 0))

# overall heat detection rate
tmp %>% 
  group_by(.id, year, i, detoest) %>%
  summarise(N = n()) %>%
  
  group_by(.id, year, detoest) %>%
  summarise(y = sum(N)) %>%
  group_by(.id, year) %>%
  mutate(y2 = sum(y),
         ratio = y/y2) %>%
  group_by(year, detoest) %>%
  summarise(hdr = mean(ratio))

# heat detection rate excluding problematic cows ( anoestrus and cod )
tmp %>% 
  filter(an_cyc1 != 1 & cod != 1) %>%
  group_by(.id, year, i, detoest) %>%
  summarise(N = n()) %>%
  group_by(.id, year, detoest) %>%
  summarise(y = sum(N)) %>%
  group_by(.id, year) %>%
  mutate(y2 = sum(y),
         ratio = y/y2) %>%
  group_by(year, detoest) %>%
  summarise(hdr = mean(ratio))

# proportion of no oestrus sign (missed because of management or suboestrus)
tmp %>% 
  filter(an_cyc1 != 1 & cod != 1 & detoest != 1) %>%
  group_by(.id, year, i, suboest) %>%
  summarise(N = n()) %>%
  group_by(.id, year, suboest) %>%
  summarise(y = sum(N)) %>%
  group_by(.id, year) %>%
  mutate(y2 = sum(y),
         ratio = y/y2) %>%
  group_by(year, suboest) %>%
  summarise(hdr = mean(ratio))

tmp2 <- MySim_cows %>%
  group_by(.id, UID, par) %>%
  mutate(incAn = if_else(lag(an_cyc1 == 0) & an_cyc1 == 1, 1, 0), 
         incCod = if_else(lag(cod == 0) & cod == 1, 1, 0),
         PPan = if_else(dim <= 86 & dim >= 65, 1, 0),
         PPcod = if_else(dim <= 105 & dim >= 65 & treated == 0 & 
                           an_cyc1 == 0, 1, 0)) %>%
  filter(detoest == 1 | ((oestrus - 21) == i & m_oest != 0)) %>%
  mutate(normalCyc = if_else(an_cyc1 == 0 & cod == 0, 1, 0)) %>%
  select(-(rhenter:rev.milk)) %>%
group_by(.id) %>%
summarise_at(.vars = c("incAn", "incCod", "PPan", "PPcod", "suboest", "normalCyc", "detoest"), .funs = sum) %>%
mutate(incAnR = incAn/PPan,
       incCodR = incCod/PPcod,
       incSub = suboest/normalCyc,
       detRate = detoest/normalCyc)
  
tmp5 <- tmp2 %>%
  select(incAnR:detRate) %>%
  summarise_all(.funs = list(
    ~mean(.),
    ~median(.),
    ~sd(.),
    ~min(.),
    ~max(.),
    ~quantile(., 0.05),
    ~quantile(., 0.95))
  ) %>% 
  pivot_longer(everything(), names_to = "v", values_to = "value") %>%
  separate(v, into = c("variable", "metric"), sep = "_")
reshape2::dcast(tmp5, formula = variable~metric)

# incidence rate

# investigate cows that spontaneously recovered
tmp4 <- MySim_cows %>%
  group_by(UID, par) %>%
  mutate(SRan = if_else(lag(an_cyc1 == 1) & an_cyc1 == 0 & lag(treated == 0) & detoest == 0, 1, 0),
         SRcod = if_else(lag(cod == 1) & cod == 0 & lag(treated == 0) & detoest == 0, 1, 0))

unique(tmp4$UID[tmp4$SRan == 1])


# CEVA outputs of interest ------------------------------------------------------------------------------------------------------------

FTAI.1_AI_descr0_np <- MySim_cows %>%
  filter(detoest == 1, pregnant == 0) %>%
  group_by(.id, year,pregnant, n_insem) %>%
  summarise(n = n()) 
  # mutate(n2 = ifelse(pregnant == 0, sum(n), n))
  # group_by(year,pregnant, n_insem) %>%
  # summarise(mu = mean(n),
  #           sigma = sd(n)) %>%
  # mutate(year = factor(year),
  #        pregnant = factor(pregnant),
  #        n_insem = factor(n_insem))
  
FTAI.1_AI_descr0_p <- MySim_cows %>%
  filter(detoest == 1, pregnant == 1) %>%
  group_by(.id, year,pregnant, n_insem, PTCL_status) %>%
  summarise(n = n()) 
  # mutate(n2 = ifelse(pregnant == 0, sum(n), n))
  # group_by(year,pregnant, n_insem, PTCL_status) %>%
  # summarise(mu = mean(n),
  #           sigma = sd(n)) %>%
  # mutate(year = factor(year),
  #        pregnant = factor(pregnant),
  #        n_insem = factor(n_insem),
  #        PTCL_status = factor(PTCL_status))

Culls2 <- MySim_cows %>%
  filter(cull == 2) %>%
  group_by(.id, cull, year) %>%
  summarise(n = n()) %>%
  mutate(n_insem = 3,
         pregnant = 0) %>% 
  ungroup() %>%
  select(c(.id, year, pregnant, n_insem, n))
  
FTAI.1_AI_descr1 <- rbind(FTAI.1_AI_descr0_p, FTAI.1_AI_descr0_np, Culls2) %>%
  mutate(PTCL_status = ifelse(pregnant == 0, 0, PTCL_status)) %>%
  group_by(year, n_insem, PTCL_status) %>%
  summarise(mu = mean(n),
            sigma = sd(n)) %>%
  mutate(year = factor(year),
         n_insem = factor(n_insem),
         PTCL_status = PTCL_status + 1)
protocols <- c("non-pregnant" = 1, "DoubleOvSynch" = 2, "OvSynch" = 3, "PridSynch" = 4, "HeatDetect" = 5)
FTAI.1_AI_descr1$PTCL_status <- names(protocols[FTAI.1_AI_descr1$PTCL_status])
FTAI.1_AI_descr1$PTCL_status <- factor(FTAI.1_AI_descr1$PTCL_status)


ggplot(data = FTAI.1_AI_descr1, aes(x = n_insem, y = mu)) +
  geom_col(aes(fill = PTCL_status)) +
  facet_grid(cols = vars(year)) +
  labs(y = "Mean number of cows", x = "Insemination number") +
  geom_text(aes(label = round(mu), group = PTCL_status), position = position_stack(vjust = 0.5)) +
  labs(caption = paste("Scenario", xPRM$CEVA_scen))

# find cows that did not get inseminated in the year
x. <- MySim_cows %>%
  filter(year != 1) %>%
  filter(oestrus == i |  detoest == 1) %>%
  group_by(.id, year, UID, pregnant) %>%
  summarise(max.ins = max(n_insem)) %>%
  group_by(.id, year, pregnant, max.ins) %>%
  summarise(n = n()) %>%
  group_by(year, pregnant, max.ins) %>%
  summarise(mu = mean(n)) 

# cows that do not recieve any inseminations during the year (conceived in the previous year)
x.1 <- MySim_cows %>%  
  filter(pregnant == 1) %>%
  group_by(.id, year, par, UID) %>%
  mutate(check1 = any(detoest == 1)) %>%
  filter(check1 == FALSE) %>%
  group_by(.id, year, UID) %>%
  slice(1) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n))


# CEVA PP Outputs of Interest ---------------------------------------------------------------------------------------------------------

# daily average cows in milk
CIM0 <- MySim_cows %>%
  filter(yield != 0) %>%
  group_by(.id, year, i) %>%
  summarise(mu = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(mu))

# averaged total milk yield per year 
MY <- MySim_cows %>%
  group_by(.id, year) %>%
  summarise(gross.milk = sum(yield),
            loss.milk  = sum (yield.rdc),
            net.milk   = sum(gross.milk) - sum(loss.milk)) %>%
  ungroup() %>%
  select(-.id) %>%
  group_by(year) %>%
  summarise_all(.funs = list(mu = ~mean(.),
                            q05 = ~quantile(., p =0.05),
                            q95 = ~quantile(., p = 0.95)))



# Number of AI's
AI0 <- MySim_cows %>%
  filter(detoest == 1) %>%
  group_by(.id, year) %>%
  summarise(n_AI = n()) %>%
  group_by(year) %>%
  summarise(mu_AI = mean(n_AI),
            sigma = sd(n_AI))

# number of protocol applications
PA0 <- MySim_cows %>%
  filter(PTCL_status %in% c(1:3), pregCheck_sdl == i) %>%
  group_by(.id, year, UID, par, PTCL_status) %>%
  slice(1) %>%
  group_by(.id, year, PTCL_status) %>%
  summarise(n = n()) %>%
  group_by(year, PTCL_status) %>%
  summarise(mu = mean(n),
            sigma = sd(n))

# number of pregnancies
NP0 <- MySim_cows %>%
  filter(detoest == 1, pregnant == 1) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n),
            sigma = sd(n))

# number of calf born
NC <- MySim_cows %>%
  filter(calf == 1) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n),
            sigma = sd(n))

# AI's per pregnancy
AI0$mu_AI/NP0$mu

# mean calving interval
CI0 <- MySim_cows %>%
  filter(calf == 1, prevdaycalf != 0) %>%
  mutate(CI = daycalf - prevdaycalf) %>%
  group_by(.id, year) %>%
  summarise(mu = mean(CI),
            sigma = sd(CI),
            q05 = quantile(CI, 0.05),
            q95 = quantile(CI, 0.95))

# distribution of calving days (possible input for youngstock model)
CD0 <- MySim_cows %>%
  filter(dim == 1) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n)) %>%
  ggplot(aes(x = day, y = mu)) +
  geom_col() +
  facet_grid(cols = vars(year))

# anoestrus prevalence at DIM
AN_prev <- MySim_cows %>%
  filter(dim == 65) %>%
  group_by(.id, year, i) %>%
  mutate(cowsDIM65 = n()) %>%
  group_by(.id, year, i, an_cyc1) %>%
  summarise(n = n()) %>%
  mutate(prop_an = an_cyc1/n) %>%
  group_by(year, an_cyc1) %>%
  summarise(mu = mean(n))

MySim_cows %>%
  filter(oestrus != 0, oestrus <= dayendvwp) %>%
  group_by(.id, year, UID) %>%
  slice(1) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n))

MySim_cows %>%
  filter(oestrus != 0, oestrus <= dayendvwp, calf != 0) %>%
  group_by(.id, year, UID) %>%
  slice(1) %>%
  ggplot(aes(x = day)) +
  geom_histogram() +
  facet_grid(rows = vars(.id), col = vars(year))

MySim_cows %>%
  filter((pregnant == 1 & detoest == 1) | oestrus  == i+21) %>%
  filter(dim %in% 65:86) %>%
  ggplot(aes(x = day)) +
  geom_histogram(aes(fill = factor(an_cyc1))) +
  facet_grid(rows = vars(.id), col = vars(year))


test0 <- MySim_cows %>%
  filter(an_cyc1 == 1) %>%
  group_by(.id, year, UID, par) %>%
  slice(1) %>%
  group_by(.id, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(mu = mean(n))

FTAI.1_AI_descr1 <- FTAI.1_AI_descr0 %>%
  mutate(year = paste("yr", year, sep = "_"),
         pregnant = paste("preg", pregnant, sep = "_"),
         PTCL_status = paste("ptcl", PTCL_status, sep = "_"), 
         Variable = paste(year, pregnant, PTCL_status, sep = "-"))
reshape2::dcast(data = FTAI.1_AI_descr1, formula = Variable~n_insem, value.var = 'n')



# Economic summaries ------------------------------------------------------


All_Vars <- colnames(MySim_cows)

# Total economic output per year
EcoSummary_test <- MySim_cows %>%
  group_by(.id, year) %>%
  select(rev.milk, grep(pattern = "cost", All_Vars, value = T)) %>%
  summarise_all(sum) %>%
  mutate(total.cost = cost.insem + cost.feed + cost.cull+ cost.milk +
           cost.milk.disc + cost.PD + cost.hormone + cost.hormone.an +
           cost.hormone.cod + cost.hormone.hi + cost.labour + cost.calf, 
         NER = rev.milk - total.cost) 

EcoSummary1 <- EcoSummary0 %>% 
  select(-.id) %>%
  group_by(year) %>%
  summarise_all(mean)

# Total economic output per year
EcoSummary_par0 <- MySimCows_00001 %>%
  group_by(.id, year, par) %>%
  select(rev.milk, grep(pattern = "cost", All_Vars, value = T)) %>%
  summarise_all(sum) %>%
  mutate(total.cost = cost.insem + cost.feed + cost.cull+ cost.milk +
           cost.milk.disc + cost.PD + cost.hormone + cost.hormone.an +
           cost.hormone.cod + cost.hormone.hi + cost.labour + cost.calf, 
         NER = rev.milk - total.cost) 


#explore economic var at cow level
MySim_cows %>%
  select(rev.milk, grep(pattern = "cost", All_Vars, value = T)) %>%
  group_by(UID, par) %>%
  summarise_all(sum)
  summarise()
  
  
#explore economic var at COW level per PARITY
EcoSum_cow_par <- MySim_cows %>%
  group_by(.id, UID, year, par) %>%
  select(rev.milk, grep(pattern = "cost", All_Vars, value = T)) %>%
  summarise_all(sum) %>%
  mutate(total.cost = cost.insem + cost.feed + cost.cull+ cost.milk +
           cost.milk.disc + cost.PD + cost.hormone + cost.hormone.an +
           cost.hormone.cod + cost.hormone.hi + cost.labour + cost.calf, 
         NER = rev.milk - total.cost) %>%
  group_by(year, par) %>%
  summarise(rev.milk = mean(rev.milk), total.cost = mean(total.cost), NER = mean (NER))