#########################################################
# Preliminary analyses for PIFSC lancetfish diet database
#########################################################

# load libraries and data ----
library(tidyverse)
library(lubridate)

# Read in raw data
prey_items_master <- read_csv("Prey_items_master.csv")
pred_sample_specific <- read_csv("Pred_sample_specific.csv")
pred_key <- read_csv("Pred_key.csv")
pred_info_master <- read_csv("Pred_info_master.csv")


# Combining datasets
prey_and_pred <- prey_items_master %>% 
  left_join(pred_sample_specific, by = "PREDATOR_ID") %>% 
  left_join(pred_key, by = c("SAMPLELOGID", "ACSAMPLEID")) %>% 
  left_join(pred_info_master, by = "SPECIMENLOGID")

# dataset of plastic ingested incidences
plastic_ingested_df <- prey_and_pred %>% 
  filter(str_detect(PREY_TAXONOMICID, "Plastic")) 

Aferox_specific <- prey_and_pred %>% 
  filter(TAXONOMICID == "Alepisaurus ferox") %>% 
  group_by(YEAR_COLLECTED) %>% 
  summarise(N = n_distinct(SPECIMENLOGID))


# Summary tables and figures----

plastic_ingested_summ <- plastic_ingested_df %>% 
  group_by(YEAR_COLLECTED) %>% 
  summarise(NwP = n_distinct(SPECIMENLOGID),
            Wgt_per_ind = median(WEIGHT_TOT)) %>% 
  left_join(Aferox_specific, by = "YEAR_COLLECTED") %>% 
  mutate(FO = NwP/N)







