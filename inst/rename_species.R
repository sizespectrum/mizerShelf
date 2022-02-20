source("init.R")
params <- readRDS("params.rds")

# Rename species
library(googledrive)
temp <- tempfile(fileext = ".xslx")
drive_download(as_id("1BmclIJ2DVWlXhuTd8B4qYC1vHEeeejnD"),
               path = temp, overwrite = TRUE)
library(readxl)
df <- read_xlsx(temp)

replace <- df$NEW
names(replace) <- df$OLD
params <- renameSpecies(params, replace)

saveRDS(params, "params.rds")
