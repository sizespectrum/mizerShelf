library(dplyr)
library(tidyr)
library(googlesheets4)
params <- readRDS("params.rds")

# We take the data from the "size-frequency" sheet in the "additional data"
# document on our Google Drive. 
catch_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1ojm6Q48bD8p7C86_AA_mNkanvDwEQSvEmp7su8aBfRU/edit#gid=1677119586",
                          sheet = 2)

catch <- catch_sheet %>% 
    pivot_longer(-cm, 
                 names_to = "latin.name", 
                 values_to = "catch",
                 values_drop_na = TRUE) %>% 
    mutate(latin.name = recode(latin.name,
                              "Lophius sp" = "Lophius spp",
                              "Micromesistius potassou" = "Micromesistiu poutassou",
                              "Trachurus sp" = "Trachurus trachurus",
                              "Trisopterus capelanus" = "Trisopterus minutus",
                              "Illex condieti" = "Illex coindietii"
                              )) %>%
    separate(cm, c("start", "end"), 
             sep = "-",
             convert = TRUE) %>% 
    mutate(length = start - 0.1,
           dl = end - length) %>% 
    left_join(params@species_params, by = "latin.name") %>% 
    select(species, latin.name, length, dl, catch) %>% 
    filter(!is.na(species))
catch
saveRDS(catch, "catch.rds", version = 2)
