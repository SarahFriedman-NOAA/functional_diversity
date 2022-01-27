# To see how Emily does this: https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/blob/main/code/data.R

## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "here")
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


## Get RACEBASE data -----------------------------------------------
# This local folder contains csv files of all the  tables.
a <- list.files(
  path = here::here("data", "oracle"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- read_csv(file = here::here("data", "oracle", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  b$database <- str_extract(a[i], "[^*]+(?=-)")
  assign(x = paste0(str_extract(a[i], "[^-]*(?=\\.)"), "0"), value = b)
  rm(b)
}



## Data wrangling -----------------------------------------------

# collecting all species information, broadest possible dataset, needs to be cleaned more for use
species <- species0 %>%
  dplyr::left_join(species_classification0, by = "species_code") %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish",
    species_code >= 40001 ~ "invert"
  )) %>%
  dplyr::filter(!grepl("shells|empty|unsorted| egg|unid\\.|shab",
                       common_name, ignore.case = TRUE)
                ) %>% # removing unwanted/unid. items
  select(species_code, species_name, common_name, taxon,
    family = family_taxon, order = order_taxon,
    class = class_taxon, phylum = phylum_taxon
  ) 


# properly cleaning up just fish names, lots of misspellings in database
species_fish <- species %>%
  filter(taxon == "fish") %>%
  filter(!grepl("sp\\.|egg|\\(juvenile\\)|larva|hybrid|iformes|idae|Ceratioidei", species_name)) %>%
  mutate(species_name = case_when(
    species_name == "Mallotus catervarius (=villosus)" ~ "Mallotus villosus",
    species_name == "Poromitra curilensis (=crassiceps)" ~ "Poromitra crassiceps",
    species_name == "Antherinopsis californiensis" ~ "Atherinopsis californiensis",
    species_name == "Rhamphocottus richardsoni" ~ "Rhamphocottus richardsonii",
    species_name == "Lycodema barbatum" ~ "Lyconema barbatum", 
    species_name == "Percis japonicus" ~ "Percis japonica", 
    grepl("(adult)", species_name) ~ gsub(" \\(adult\\)", "", species_name),
    TRUE ~ species_name
  ))


# making a clean specimen level data frame with taxonomic information and up-to-date length info
specimen <- length0 %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female", 
    . = TRUE ~ "unsexed"
  )) %>%
  full_join(species_fish, by = "species_code") %>%
  dplyr::select(
    cruisejoin, hauljoin, catchjoin, region, species_name, taxon, 
    length, length_type, sex,
    species_code, common_name, family:phylum
  ) %>%
  mutate(length_type = case_when(
    length_type == 1 ~ "fork length",
    length_type == 2 ~ "mideye to fork length",
    length_type == 3 ~ "standard length",
    length_type == 4 ~ "mideye to hypural plate",
    length_type == 5 ~ "total length",
    length_type == 6 ~ "snout to second dorsal",
    length_type == 7 ~ "carapace from back of right eye socket to end of carapace",
    length_type == 8 ~ "carapace width",
    length_type == 9 ~ "head length",
    length_type == 11 ~ "pre-anal length",
    length_type == 12 ~ "mantle length",
    length_type == 13 ~ "posterior of orbital to end of telson",
    length_type == 14 ~ "wingtip to wingtip",
    length_type == 15 ~ "outer tip of rostrum to end of telson",
    length_type == 16 ~ "modal length",
    length_type == 17 ~ "Length frequency estimated using size composition proportions from adjacent hauls with similar catch composition"
  )) #%>%
  #filter(region %in% c("AI", "GOA", "BS")) #only want Alaska


# Cleaning cruise info from RACE.DATA
# AFAIK (Megsie) you can only get the name of the survey from the cruises.csv file, which is from RACEDATA
cruise <- v_cruises0 %>%
  dplyr::select(cruise_id, year, survey_name, vessel_id, cruise,
                survey_definition_id, vessel_name, start_date, 
                end_date, cruisejoin) %>% 
  dplyr::filter(year >= 1982 & year != 2020) #no cruises happened in 2020



# standard filters for haul data set among RACE folks, dropping rows with missing coordinates
haul <- haul0 %>%
  dplyr::filter(performance >= 0 & # only "good" hauls
    haul_type == 3 & # only standard bottom trawl
    abundance_haul == "Y"&
     # region %in% c("GOA", "AI") &
      !is.null(stationid)) %>% # hauls used in abundance estimates
  dplyr::filter(!is.na(start_latitude) & !is.na(start_longitude)) %>% 
  dplyr::select(
    cruisejoin, hauljoin, region, surface_temperature, gear_temperature,
    bottom_depth, gear_depth, start_latitude, start_longitude, start_time
  )


# combining haul & cruise information
cruise_haul <- left_join(haul, cruise, by = c("cruisejoin"))


gap_data <- cruise_haul %>%
  full_join(specimen) %>%
  select(-gear_temperature, -start_time, -cruise_id, -c(survey_name:end_date)) %>%
  select(species_name, common_name, taxon, region, length, length_type, sex,
         surface_temp = surface_temperature, bottom_depth, everything()) %>%
  filter(!is.na(species_name)) #removing inverts and unid sp
