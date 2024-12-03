setwd("C:/Users/6350089/OneDrive - Universiteit Utrecht/Side project/Ceva_model environment/Cow simulation")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

xPRM <- list()

# Generic inputs ----------------------------------------------------------

xPRM$storeWD <- paste0("D:/New_cow_sensitivity")  #must set on grandparent directory

xPRM$n_cows <- 200   # number of cows in herd
xPRM$n_sim  <- 500  # number of simulations 
xPRM$n_yrs  <- 20    # time horizon in years
xPRM$ts     <- 365   # time-steps per year

source("ProdMod_internalFunc.R")


# Production inputs -------------------------------------------------------

xPRM$max_par      <- 10                                   # maximum parity for last lactation (Authors' expertise)
xPRM$use_par      <- 6                                    # expected lactaions of cow - depreciation over useful life mkFrames (Authors expertise)
xPRM$p_par        <- setNames(c(0.29, 0.25, 0.19, 0.13, c(rep(0.15/6, 6))), paste0("par", seq(xPRM$max_par))) # preliminary probabilty of a cow being in 1:max_par (CRV, 2022)
# xPRM$p_dtoes      <- 0.60                                 # probability of oestrus detection (Based on Rutten et al., 2014; doi:10.3168/jds.2014-7948) 
#xPRM$p_conc       <- c(0.45, 0.42, 0.41, 0.38, 0.33, 0.27) # success of insemination for insemination number (Inchiasri et al., 2011; doi:10.1111/j.1439-0531.2011.01782.x)
xPRM$p_stlbrn     <- 0.0                                  # probability of cow having a still born [preliminary 20/11/19]
xPRM$p_gencull    <- c(0.01, 0.01, 0.03, 0.4, 1)        # calibrated so that overall culling rate ~30%  
xPRM$p_mort       <- 0.00                                 # mortality rate of general cull cows (Authors' expertise)
xPRM$p_rh         <- 0.3                                  # calibrated probability that a replacement heifer will enter the herd on the following day that a cow dies (geometric distribution: rgeom())
xPRM$yield_thresh <- 15                                   # kg/day; infertile cow will be culled if producing below this parameter (Authors' expertise)
# xPRM$VWP          <- 65                                   # voluntary waiting period in days before next AI (Inchiasri et al., 2010; doi:10.1016/j.theriogenology.2010.04.008)
xPRM$DPL          <- 56                                   # dry period length in days (Inchiasri et al., 2010; doi:10.1016/j.theriogenology.2010.04.008)
xPRM$ADG          <- ((650-540)/2)/410                    # average daily gain (Based on Kok et al., 2017; https://doi.org/10.1371/journal.pone.0187101)
daysOpen.mat <- data.table::fread("daysOpen_mat.csv")

xPRM$yieldQuants <- quantile(daysOpen.mat$mean.rpl, c(0.25, 0.50, 0.75, 1))

#input for probability of conception rate, refer to Inchaisri et al 2011, Table 2:

xPRM$cr_intc       <- -0.3
xPRM$cr_AI         <- c(0, 0.13, 0.08, -0.02, -0.37, -0.76)
xPRM$cr_par        <- c(0.21, 0.15, 0.11, 0.03, 0)
xPRM$cr_breed      <- 0 #100%HF
xPRM$cr_lastcalf   <- -0.025 #half male and female
xPRM$cr_AIseason   <- c(0.02, -0.05, 0.04, 0)
xPRM$cr_peak       <- c(-0.28, 0) # AI time (before peak yield, after peak yield)
xPRM$cr_DIM        <- 0.005
xPRM$cr_MY         <- -0.00009 #Milk yield at AI date
xPRM$cr_MY.DIM     <- -0.00007
xPRM$cr_season.DIM <- c(-0.0006, -0.0011, -0.001, 0)
xPRM$cr_AI.DIM     <- c(0, -0.0022, -0.0021, -0.0022, -0.0011, -0.0011)
xPRM$cr_par.DIM    <- c(-0.0007, -0.0001, 0.0003, 0.0006, 0)




# milk yield parameters (Kok et al., 2017; https://doi.org/10.1371/journal.pone.0187101) 
# parameter vectors are arranged according to parity order
xPRM$a   <- c(31.6, 40.6, 44.1)
xPRM$b   <- c(-0.0447, -0.0708, -0.0835)
xPRM$d   <- -16.1
xPRM$K   <- 0.06
xPRM$ady <- with(xPRM, 
                 sapply(1:length(a), function(x){mean(a[x] +b[x] * 1:305 + d *exp(-K * 1:305))})) # average daily 305-d milk yield in kg
xPRM$fat  <- c(0.0448, 0.045, 0.0451, 0.0451,  0.0451, 0.0451, 0.0451, 0.0451,  0.0451, 0.0451)  # fat content
xPRM$prot <- c(0.0355, 0.0359, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351)   # protein content

# energy requirements
xPRM$G.ENGY <- c(660, 330)              # growth energy for cows in parity 1 & 2 (van Es, 1978; https://doi.org/10.1016/0301-6226(78)90029-5)
xPRM$P.ENGY <- c(2700, 1500, 850, 450)  # energy during stage of pregnancy in order of last month, to fourth month before calving (Remelink et al., 2016; table 6.10 Dairy farming handbook)


# CEVA parameters ---------------------------------------------------------------------------------------------------------------------
# 
xPRM$CEVA_scen      <- 3
xPRM$`p_CL+`         <- 0.7                                 # probability of corpus luteum
xPRM$PR_dblOvSynch <- 0.45
# probability of conception after double ovsynch protocol
xPRM$PR_OD        <- 0.35                                 # probability of conception after heat detection (scenario 2, 3), the best knowledge we have for intensive hormone protocols (Santos et al 2017)
xPRM$PR_OvSynch   <- 0.35                                 # probability of conception afer OvSynch
xPRM$PR_Prid      <- 0.35                                 # probability of conception afer PRID synch protocol
#xPRM$p_conc       <- if(xPRM$CEVA_scen %in% c(1, 2, 3)){c(xPRM$PR_dblOvSynch, xPRM$PR_OvSynch, xPRM$PR_Prid, xPRM$PR_OD)}else{xPRM$p_conc}
xPRM$p_conc       <- if(xPRM$CEVA_scen %in% c(1, 2)){c(xPRM$PR_dblOvSynch, xPRM$PR_OvSynch, xPRM$PR_Prid, xPRM$PR_OD)}else{xPRM$p_conc}
# xPRM$p_conc       <- ifelse(xPRM$CEVA_scen %in% c(1, 2, 3), 
#                             c(xPRM$PR_dblOvSynch, xPRM$PR_OvSynch, xPRM$PR_Prid), xPRM$p_conc)
xPRM$p_dtoes      <- 0.3     # sensitivity                             # Scenarios (1:FTAI without heat detection) ~ 1; (2:FTAI with heat detection) ~ prob; (3: Heat detection) ~ prob.     
xPRM$VWP          <- 65                                   # voluntary waiting period in days before next AI (Inchiasri et al., 2010; doi:10.1016/j.theriogenology.2010.04.008)
xPRM$t_dblOvSynch <- 27                                   # Protocol duration
xPRM$t_dblOvSynch2 <- 12                                   # insemination interval after the End VWP of 65 = (27-(65-50)) = 12; where 65 = VWP, 50 = start of DblOvs protocol
xPRM$t_obs         <- 30                                  # observation days to pregnancy diagnosis
xPRM$t_OvSynch    <- 10                                   # Protocol duration
xPRM$t_PridSynch  <- 10                                   # Protocol duration
xPRM$max_insem    <- 3                                    # culling rule: maximum number of inseminations 
xPRM$t_oest       <- 21:24                                # oestrus cycle length (days)
xPRM$strict_pdINT <- F


# Baseline parameters -----------------------------------------------------
xPRM$baseline_int <- T
xPRM$t_hormone_an   <- 10
xPRM$t_hormone_cod   <- 10
xPRM$p_dtoesHI       <- 0.51 # Stevenson et al (1989)
xPRM$p_concH <- c(0.35, 0.35, 0.414) #conception rates based on hormone treatment (where rates in position 1 = anoestrus treatment, 2 = COD treatments)
xPRM$p_subselect <- 0.2 

# Reproductive disorder parameters -----------------------------------------
xPRM$RC_cows       <- 0.795                                  # probability of resuming cyclicity; Opsommer et al. (2000)
xPRM$COD_cows      <- 0.085                                  # Probability of COD; Laporte et al. (1994)
xPRM$SUB_cows      <- 0.15                                   # Van Eerdenburg et al. (2002)
# post partum (pp) period of occurence
xPRM$pp_an <-   65    # up to `pp_an` days
xPRM$pp_cod <- 105   # within `pp_cod` days 
# xPRM$pp_cyc_an  <- 1.5  # average number of cycles during anoestrus pp period
# xPRM$pp_cyc_cod <- NA  # number of cycles during cod pp period
xPRM$sr_an      <- 0.3 # spontaneous recovery rate of anoestrus episode  (Parkinson et al. 2019) 
xPRM$sr_cod      <- 0.3 # spontaneous recovery rate of cod episode       (Parkinson et al. 2019)


# Risk factors ------------------------------------------------------------
# re. resuming cyclicity
xPRM$anRF_par    <- c(0.48, 1)/((1 - 0.805) + (0.805 * c(0.48, 1))) # multiparous cows are referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014)
xPRM$anRF_yield  <- c(1, 1.34, 1.36, 1.21)/((1-0.727) + (0.727 * c(1, 1.34, 1.36, 1.21))) # lower quartile is referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014)
xPRM$anRF_season <- c(0.65, 1, 0.83, 0.54)/((1-0.841) + (0.841 * c(0.65, 1, 0.83, 0.54))) # summer is referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014)

# xPRM$anRF_par   =xPRM$anRF_par   /xPRM$anRF_par   
# xPRM$anRF_yield =xPRM$anRF_yield /xPRM$anRF_yield 
# xPRM$anRF_season=xPRM$anRF_season/xPRM$anRF_season

# COD incidence
xPRM$codRF_par     <-  c(1, 1.37, 0.95, 0.9, 0.73, 0.47, 0.23, 0.2) # (Laporte et al, 1994)
xPRM$codRF_season  <-  c(0.54,1,1.8,1) # (Nelseon et al. 2010 : doi:10.1186/1751-0147-52-60)

# conception rate 58 days post AI
xPRM$conRF_par <- c(1.31, 1)/((1 - 0.297) + (0.297 * c(1.31, 1))) # multiparous are referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014) 
xPRM$conRF_cyc <- c(1.67, 1)/((1 - 0.245) + (0.245 * c(1.67, 1))) # anoestrus are referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014)
xPRM$conRF_cyc_rvs <- c(1, 0.59)/((1 - 0.245) + (0.245 * c(1, 0.59))) # cycling cows are referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014) 
xPRM$conRF_soc <- c(1.26, 1, 0.97, 1.2)/((1 - 0.303) + (0.303 * c(1.26, 1, 0.97, 1.2))) # summer is referent (Santos et al (2009): doi:10.1016/j.anireprosci.2008.01.014) 
xPRM$conRF_cod <- c(1, 0.88)                                      # Fourichon et al. (2000)

#calib_CR <- 0.8

# oestrus detection 
xPRM$odRF_rpl <- c(1.1, 1, 0.9)  # apropos relative production level (<0.9, 0.9-1.1, >1.1) Inchiasri et al (2010) 
xPRM$peakYieldDay <- with(xPRM, 
                          sapply(lapply(1:3, function(j){ (a[j] + b[j] * 1:305 + d * exp(-K * 1:305)) }), which.max))


# Production impact -------------------------------------------------------

xPRM$mloss_1 <- 0.28
xPRM$mloss_2 <- 0.04   #best fit according to (Olori 1997)

# Economic inputs ---------------------------------------------------------

xPRM$cost.EkVEM  <- 0.209  # 06-2020 - 06-2022 (WECR, 2022; https://www.wur.nl/nl/Onderzoek-Resultaten/Onderzoeksinstituten/livestock-research/Producten/Voederwaardeprijzen-Rundvee.htm)
xPRM$cost.insem  <- 20     # cost per insemination (Blanken et al., 2021; KWIN)

# culling costs
#xPRM$cost.rear  <- c(919, 1790, 3307)    # min, mean, max rearing costs (Nor et al., 2015; https://doi.org/10.1186/s13620-015-0058-x)
xPRM$cost.rear  <- c(1790, 1790, 1790)    # use average rearing costs (Nor et al., 2015; https://doi.org/10.1186/s13620-015-0058-x)
xPRM$perc.dress <- 0.6                   # carcass dressing percentage (Rutten et al., 2014; doi:10.3168/jds.2014-7948.)
xPRM$price.kg   <- c(3.68, 3.68, 3.68)   # 2nd grade meat price - Wageningen Economic Research, 2022. Agricultural Prices. https://agrimatie.nl/Prijzen.aspx?ID=15125. (average  June 2020- June 2022)

# Milk revenue
#xPRM$MilkPrice <- 0.3502 # Wageningen Economic Research, 2020. Agricultural Prices. https://agrimatie.nl/Prijzen.aspx?ID=15125. c/kg average 2016 - may 2020
xPRM$MilkPrice <- 40.22 # Wageningen Economic Research, 2022. Agricultural Prices. https://agrimatie.nl/Prijzen.aspx?ID=15125. c/kg average june 2020 - june 2022

xPRM$RemCost <- 39 # Rendac (2021) https://www-cambridge-org.ezproxy.library.wur.nl/core/services/aop-cambridge-core/content/view/7977B21E1B8F78E002AE0A1196CA98D8/S1751731117001306a.pdf/estimating_the_economic_impact_of_subclinical_ketosis_in_dairy_cattle_using_a_dynamic_stochastic_simulation_model.pdf  

# Hormone costs
xPRM$cost.hormone   <- c(24.5, 14, 28.55) #cost DoubleOvSynch (1), OvSynch (2), PridSynch (3)
xPRM$cost.hormone.an  <- 25.05
xPRM$cost.hormone.cod <- 10.5
xPRM$cost.hormone.hi  <- 3.5
# Pregnancy diagnosis
xPRM$cpd_cof <- 31.35      # call out fee vet
xPRM$cpd_lb0 <- 0       # hourly rate respective of farmer agent fetching cows
xPRM$cpd_lb1 <- 139.2       # hourly rate respective of agen diagnosing cows       
xPRM$lpd_ftc <- 1    # average time farmer to fetch cows for PD (headlock system) (min/cow)
xPRM$lpd_dur <- 5    # vet labour time per diagnosis per cow (min/cow)
# General labour inputs
xPRM$t_labDoubleOv  <- 7 #accumulative duration for protocol in minute (7 injections * 1 minute)
xPRM$t_labOvSynch   <- 4 #accumulative duration for protocol in minute (4 injections * 1 minute)
xPRM$t_labPridSynch <- 4 #accumulative duration for protocol in minute (4 injections * 1 minute)
xPRM$t_labPridSynchAN <- 3 #accumulative duration for protocol in minute (3 injections * 1 minute)
xPRM$t_labOvSynchCOD <- 3 #accumulative duration for protocol in minute (3 injections * 1 minute)
xPRM$t_labHI         <- 1

xPRM$t_labAI <- 10  # duration of AI in minute
xPRM$cost_lab       <- 28.85  # farmer labour cost per hour (Blanken et al 2021)

# calving costs related inputs
#xPRM$p_mortC <- c(0.07, 0.01) # KWIN 2021
xPRM$p_mortC <- c(0.033, 0.033) #Santman-Berends et al 2019

xPRM$w_0 <- 40.17
xPRM$v_0 <- sqrt(10.18)
xPRM$w_m <- 510
xPRM$k_0 <- 0.14

xPRM$WAB    <- expression(rnorm(length(cd), w_0, v_0))

#calving economic inputs
xPRM$bw_milkreplace <- 42  # body weights defining the milk replacement diet  (there are 4 diets; diet one and two are determined by the body weight at birth)
xPRM$l_milkreplace  <- c(4, 5)
xPRM$cost_milkreplace <- 2   # cost per kg of milk replacement (Blanken et al 2021)
xPRM$t_labcalf      <- c(1, 5, 5, 1) # min/day/calf to feed and check in summer:spring (Nor et al., 2014/2015)
xPRM$rem_calf       <- 26 # carcass removal price for dead heifer (Rendac 2021)
xPRM$noinsem_int  <- 60       # interval for previous or no insemination  after VWP --> disease diagnosis
xPRM$noinsem_int2  <- 30       # interval for intervention after previous insemination/treatment --> disease diagnosis
xPRM$cost_calfmanage <- 152 # calving management cost (Inchaisri et al 2010)
xPRM$calf.revenue    <- 64.29  # mean price of bull and heifer, Wageningen Economic Research, 2022. Agricultural Prices. https://agrimatie.nl/Prijzen.aspx?ID=15125. c/kg average june 2020 - june 2022

# Other -------------------------------------------------------------------

xPRM$validate    <- TRUE
xPRM$seed <- 123 #it is a random number, you can change the seed with other numbers to have different results from more iterations
xPRM$last.update <- date()


# Save inputs -------------------------------------------------------------
save(xPRM, file = "reprodAction_defaultParameters.RData") #~ *NB* remember to save the input data as an .Rdata file every time you add inputs in this script by running this line of code

# xPRM <- mget(load("default_Parameters.RData", envir=(NE. <- new.env())), envir=NE.)

# sink("default_Parameters.txt")
# print(xPRM)
# sink()
