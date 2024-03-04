
## SCRIPT TO RUN MODELS FOR ALL SPECIES AT ONCE 
## WARNING: TAKES A VERY LONG TIME (> 30-40 HOURS) TO RUN AT FULL ITERATIONS (at least on my computer)

library(tidyverse)
library(R2jags)
library(parallel)

# READ IN COVARIATE DATA FILES 
herbs_array <- read_rds(file = "Data/Model Input Files/herbaceous_cover.rds")
rsd_array <- read_rds(file = "Data/Model Input Files/relative_stand_density.rds")
cumulative_precip_array <- read_rds(file = "Data/Model Input Files/Cummulative_summer_precip.rds")
jday_array <- read_rds(file = "Data/Model Input Files/JulianDate.rds")
precip_array <- read_rds(file = "Data/Model Input Files/DailyPrecipDetprob.rds")
temp_array <- read_rds(file = "Data/Model Input Files/DailyTempDetprob.rds")
trap_df <- read_csv("Data/Model Input Files/trap_number.csv")
tmin_dev <- read_csv("Data/Model Input Files/WinterTMinDev_2023.csv")
hdat <- read_csv("Data/Model Input Files/NOAA_hdays_2023.csv")
mast_df <- read_csv("Data/Model Input Files/HB_Beech_Mast_2022.csv")

# VECTOR OF THE NUMBER OF SITES TRAPPED PER YEAR
sitesVec <- c(4,4,4,4,
              8,8,8,8,8,8,
              9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9)

# SPECIES TO RUN MODELS FOR
spp_vec <- c("PELE", 
             "PEMA", 
             "SOCI", 
             "SOHO", 
             "SOFU", 
             "BLBR", 
             "MYGA", 
             "NAIN", 
             "MIPE")

# MODEL FILE TO RUN FOR EACH SPECIES
model_vec <- c("Model TXT Files/Poisson with OD.txt", # PELE
               "Model TXT Files/ZIP with OD.txt",     # PEMA
               "Model TXT Files/ZIP with OD.txt",     # SOCI
               "Model TXT Files/Poisson.txt",         # SOHO 
               "Model TXT Files/Poisson.txt",         # SOFU
               "Model TXT Files/ZIP with OD.txt",     # BLBR
               "Model TXT Files/ZIP with OD.txt",     # MYGA
               "Model TXT Files/ZIP with OD.txt",     # NAIN
               "Model TXT Files/Poisson.txt")         # MIPE        

# EMPTY LIST TO FILL WITH MODEL OUTPUT
my_list <- list()

# LOOP TO RUN ALL MODELS
for(i in spp_vec){
  
  y <- read_rds(file = paste0("Data/Count Data/", i, "_Counts_Array.rds"))
  
  # Find the total number of individuals captured each day
  ncap <- apply(y, 1:3, sum)
  
  # Bundle data for JAGS
  win.data <- list(y = y,
                   n = ncap,
                   mast = mast_df$mean_beech_m2,
                   summerprecip = cumulative_precip_array,
                   tmin = tmin_dev$mean_TMIN_deviation,
                   hdays = hdat$heimal_days,
                   rsd = rsd_array,
                   herbs = herbs_array,
                   site = trap_df$COMP,
                   temp = temp_array,
                   precip = precip_array,
                   jday = jday_array,
                   winsev = winter_sev$max_sev_scale,
                   lgTrapNumber = log(trap_df$TrapNo),
                   sitesVec = sitesVec)
  
  # Select the correct model file for each species
  model_index <- which(spp_vec == i)
  model_file <- model_vec[model_index]
  
  # PARAMETERS TO MONITOR
  params <- c("b.mast",
              "b.site",
              "b.summerprecip",
              "b.tmin",
              "b.hdays",
              "b.rsd",
              "b.herbs",
              "mu.site",
              "mu.site.p",
              "p.site",
              "p.temp",
              "p.precip",
              "p.jday",
              "fit.obs",
              "fit.new",
              "b.pvalue",
              "yearlyN")
  
  # Run Model
  out <- jags.parallel(data = win.data, 
                       inits = function()list(N = apply(y,c(1,2,3),sum) + 2), 
                       parameters.to.save = params, 
                       model.file = model_file,
                       n.chains = 3, 
                       n.thin = 30, 
                       n.iter = 500000, 
                       n.burnin = 200000, 
                       working.directory = getwd(),
                       n.cluster = 3)
  
  # Append model output to list
  my_list[[i]] <- out
  
}

# SAVE MODEL OUTPUTS
saveRDS(my_list$PELE, file = "Model Output/PELE.rds")
saveRDS(my_list$PEMA, file = "Model Output/PEMA.rds")
saveRDS(my_list$SODI, file = "Model Output/SODI.rds")
saveRDS(my_list$SYCO, file = "Model Output/SYCO.rds")
saveRDS(my_list$SOCI, file = "Model Output/SOCI.rds")
saveRDS(my_list$SOHO, file = "Model Output/SOHO.rds")
saveRDS(my_list$SOFU, file = "Model Output/SOFU.rds")
saveRDS(my_list$BLBR, file = "Model Output/BLBR.rds")
saveRDS(my_list$MYGA, file = "Model Output/MYGA.rds")
saveRDS(my_list$NAIN, file = "Model Output/NAIN.rds")
saveRDS(my_list$MIPE, file = "Model Output/MIPE.rds")



