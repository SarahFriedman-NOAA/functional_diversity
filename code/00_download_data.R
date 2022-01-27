## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "RODBC", "here")
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


## Connect to Oracle -------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
if (!exists("channel")) {
  source("C:/Users/sarah.friedman/Work/Rfunctions/ConnectToOracle_STF.R")
}


## Download data sets to local machine -------------------------------------------------------
if (!file.exists("data/oracle")) dir.create("data/oracle", recursive = TRUE)


# what files are already on local machine?
x <- askYesNo(msg = "Would you like to skip downloading Oracle tables already present on your local computer? This will save time.")
if (x) {
  # checking for local files so no redundant time consuming downloads
  local_files <- str_extract(list.files("data/oracle/", recursive = TRUE), "[^/]*(?=\\.)")
} else {
  local_files <- NULL
}


locations <- c(
  ##General Tables of data (racebase)
  "RACEBASE.CATCH",
  "RACEBASE.HAUL",
  "RACEBASE.LENGTH", #DO NOT USE THIS
  "RACEBASE.SPECIMEN",
  "RACEBASE.STRATUM",
  "RACEBASE.STATIONS",
  "RACEBASE.SPECIES",
  "RACEBASE.SPECIES_CLASSIFICATION",
  ## Race Data tables
  "RACE_DATA.V_EXTRACT_FINAL_LENGTHS",
  # "RACE_DATA.HAULS", #don't use!
  "RACE_DATA.RACE_SPECIES_CODES",
  "RACE_DATA.VESSELS",
  "RACE_DATA.TAXONOMIC_RANKS",
  "RACE_DATA.SPECIES_TAXONOMICS",
  "RACE_DATA.V_CRUISES",
  ## Alaska Dept Fish and Game (ADFG)
  #"RACEBASE.LENGTH_ADFG",
  #"RACEBASE.SPECIMEN_ADFG",
  #GOA
  "GOA.GOA_STRATA"
)


for (i in 1:length(locations)) {
  print(locations[i])
  filename <- tolower(gsub("\\.", "-", locations[i]))
  if(filename %in% local_files){
    cat(paste0(locations[i], " already present locally. Skipping download.\n\n"))
  } else {
    a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
    write_csv(
      x = a,
      here("data", "oracle", paste0(filename, ".csv"))
    )
    remove(a)
  }
}

