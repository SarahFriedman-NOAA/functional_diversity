## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "here")

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


# load functions into environment
source(here("code/functions.R"))


# If local copy of racebase tables does not exist, retrieve one -----------
if(!file.exists("data/oracle")){
  x <- askYesNo(msg = "No local oracle tables detected in this directory. Would you like to download local copies now?")
  if(x){
    source(here("code/00_download_data.R"))
  }
} 



## Prep data ---------------------------------------------------------------
source(here("code/01a_clean_survey_data.R"))
source(here("code/01b_get_ecology_data.R"))


# finalized objects from these files are:
  # eco_clean -- ecological data for all species
  # trimphy -- phylogeny matching all species in eco_clean
  # gap_data -- survey data for all species at the specimen level, including abiotic vars and coords



## PCA ---------------------------------------------------------------


