
## SCRIPT FOR CREATING DAILY COUNT DATA CSV FILES FOR EACH SPECIES IN DATASET ##

library(tidyverse)
library(lubridate)


bef <- read_csv("Data/BEF_USFS_Pitfall Data_Long_All Years.csv")

# Create template df with row for each year (1994-2023), site (1-9), and trap period (1-3)
YEAR <- as.data.frame(c(1994:2023))
COMP_I <- as.data.frame(c(1:9))          # Has to be an easier way to do this...
TP <- as.data.frame(c(1:3))

full_df <- merge(YEAR, TP) 

full_df <- merge(full_df,COMP_I) %>% 
  rename(YEAR = 1,
         TP = 2,
         COMP_I = 3)

# Species to analyze
spp_vec <- c("PELE", 
             "PEMA", 
             "SOCI", 
             "SOHO", 
             "SOFU", 
             "BLBR", 
             "MYGA", 
             "NAIN", 
             "MIPE")


# Loop for creating rds file for each species daily counts in the correct array format
for(i in spp_vec){
  
  dets_by_period <- bef %>% 
                      filter(YEAR > 1993) %>%
                      # Remove extra TPs in 2014 & 2015
                      filter(TP != 0) %>% 
                      mutate(COMP = ifelse(COMP == "45a", "45A",
                                      ifelse(COMP == "45b", "45B", COMP))) %>% 
                      filter(SPECIES == i | SPECIES == "NONE") %>% 
                      mutate(COUNT = ifelse(SPECIES == "NONE", 0, COUNT)) %>% 
                      # Convert day scale to 1-10 for each trap session
                      mutate(DAY = case_when(
                        DAY < 11 ~ DAY,
                        DAY > 10 & DAY < 21 ~ DAY-10,
                        DAY > 20 ~ DAY-20),
                        COUNT = as.numeric(COUNT)) %>%
                      group_by(YEAR, COMP, DAY, TP) %>%                        
                      summarise(Caps = sum(COUNT)) %>% 
                      ungroup() %>% 
                      select(YEAR, COMP, TP, DAY, Caps) %>% 
                      arrange(YEAR, COMP, TP, DAY) %>% 
                      mutate(YEAR = as.integer(YEAR),
                             COMP = as.factor(COMP),
                             COMP_I = as.integer(COMP),
                             TP = as.integer(as.factor(TP)),
                             DAY = as.integer(as.factor(DAY)),
                             DAY = paste0("Day", DAY)) %>% 
                      pivot_wider(id_cols = c(YEAR, COMP, COMP_I, TP), 
                                  names_from = DAY, 
                                  values_from = Caps) %>% 
                      mutate_all(~replace(., is.na(.), 0)) 
  
  # Join detection data back to full df template to include NAs
  dets <- left_join(full_df, dets_by_period) 
  
  # Save file
  write.csv(file = paste0("Data/Count Data/", i, "_dets_ALL.csv"), dets, row.names = F)

}

# Create RDS files (arrays) 

# LOOP TO RUN ALL MODELS
for(i in spp_vec){
  
  # Read in and format species data
  df <- read_csv(paste0("Data/Count Data/", i, "_dets_ALL.csv"))
  
  df <- df %>%
    # Fill in NAs in the COMP column
    mutate(COMP = case_when(COMP_I == "1" ~ "36",
                            COMP_I == "2" ~ "41",
                            COMP_I == "3" ~ "42",
                            COMP_I == "4" ~ "45",
                            COMP_I == "5" ~ "45A",
                            COMP_I == "6" ~ "45B",
                            COMP_I == "7" ~ "5&6",
                            COMP_I == "8" ~ "7&8",
                            COMP_I == "9" ~ "CC")) %>%
    # Convert year to proper scale (1 - 29)
    mutate(YEAR = (YEAR - 1993),
           # Manually rename COMPS in order they were established
           COMP_I = case_when(COMP == "5&6" ~ 1,
                              COMP == "CC" ~ 2,
                              COMP == "42" ~ 3,
                              COMP == "45" ~ 4,
                              COMP == "36" ~ 5,
                              COMP == "41" ~ 6,
                              COMP == "45A" ~ 7,
                              COMP == "45B" ~ 8,
                              COMP == "7&8" ~ 9)) %>%
    # Drop extra COMP column & rename remaining column
    select(-c(COMP)) %>%
    rename("COMP" = "COMP_I")
  
  # Reorder columns so NA's are at "end" (not sure that's necessary)
  df <- df %>%
    arrange(YEAR, TP, COMP)
  
  # Set up empty array for the observations
  y <- array(NA,dim=c(30,3,9,10))               # 29 years, 3 sessions, 9 sites, 10 days
  
  # Fill the empty array with the data
  for(m in 1:30){
    for(k in 1:3){
      sel.rows <- df$YEAR == m &
        df$TP == k
      y[m,k,,] <- as.matrix(df)[sel.rows,4:13]
    }
  }
  
  write_rds(y, file = paste0("Data/Count Data/", i, "_Counts_Array.rds"))
  
}
y
