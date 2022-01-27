# building up multivariate functional database with other sources (fishbase, other AFSC groups, publications, etc.)

# Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "here", "rfishbase", "phytools", "janitor")
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


# Data wrangling -----------------------------------------------

# species level data for just identified bony fishes
clean_species <- specimen %>%
  filter(class == "Actinopterygii") %>% # just limiting to bony fishes
  filter(!grepl("sp\\.|hybrid", species_name)) %>%
  select(species = species_name, family:order) %>%
  mutate(species = replace(
    species,
    species == "Mallotus catervarius (=villosus)",
    "Mallotus villosus"
  )) %>%
  filter(species != "Bothrocara zestum") %>%
  unique() %>%
  arrange(family, species) %>%
  mutate(fishbase_name = purrr::map(species, get_fishbase_name)) %>%
  unnest(cols = c(fishbase_name))


# bringing in the phylogeny
rawphy <- read.tree(here("data/rabosky_phylo.tre"))

# fixing phylogeny tips and making sure they are up to date
updated_tips <- get_fishbase_name(rawphy)
stopifnot(all(rawphy$tip.label == updated_tips$tree_name))
stopifnot(length(rawphy$tip.label) == nrow(updated_tips))

# phylogeny with fixed tip labels
fixphy <- rawphy
fixphy$tip.label <- updated_tips$new_name


# which species are still not in the phylogeny?
clean_species$fishbase_name[which(!clean_species$fishbase_name %in% fixphy$tip.label)]


# phylogeny trimmed to species in dataset
trimphy <- drop.tip(fixphy, setdiff(fixphy$tip.label, clean_species$fishbase_name))



# Getting fishbase ecological and species data --------------------------------------
fb_data <- clean_species %>%
  mutate(fishbase_name = gsub("_", " ", clean_species$fishbase_name)) %>%
  mutate(
    sp_data = purrr::map(fishbase_name, rfishbase::species),
    eco_data = purrr::map(fishbase_name, rfishbase::ecology)
  ) %>%
  unnest(cols = c(sp_data, eco_data), names_repair = "universal") %>%
  select(species:fishbase_name,
    column_pos = DemersPelag,
    depth_min = DepthRangeShallow, depth_max = DepthRangeDeep,
    migration = AnaCat, length = Length, length_type = LTypeMaxM,
    diet = FeedingType, Comments, AddRems
  )




# Getting diet data from NOAA -----------------------------------------------

# downloaded data from https://apps-afsc.fisheries.noaa.gov/refm/reem/webdietdata/dietdataintro.php
raw_diet <- tibble(path = list.files(here("data/REEM_data/"), pattern = "Diet", full.names = TRUE)) %>%
  mutate(file = purrr::map(path, read_csv)) %>%
  select(file) %>%
  unnest(cols = c(file)) %>%
  clean_names() %>%
  filter(pred_sex != 3) %>% #removing juveniles
  select(year:day, region, prey_name, pred_name)


fish_prey <- c("Misc Teleost", "Walleye pollock", "Lepidopsetta sp",
               "Flathead sole", "Agonidae", "Pholidae", "Salmonidae", 
               "Southern rock sole", "Hippoglossoides spp", "Myctophidae",
               "Cottid", "Ammodytidae", "Zoarcoidae", "Misc Flatfish", "Misc Hexagrammidae", 
               "Pacific halibut", "Kamchatka flounder", "Macrouridae", "Yellowfin sole",
               "Atka Mackeral", "Cyclopteridae", "Stichaeidae", "Sablefish", "Pacific Cod",
               "Misc Gadidae", "Rajadae", "Greenland turbot", "Alaska plaice",
               "Bathylagid", "Arrowtooth flounder", "Unid Rockfish", "Clupeoidei",
               "Northern rock sole", "Pacific sandfish", "Bering Flounder", "Sebastelobus",
               "Misc Non-teleost fish", "Sebastes", "Osmerid")
decapod_prey <- c("Misc Crab", "Tanner Crab", "Red King Crab", "Blue King Crab",
               "Misc Decapoda", "Cancridea", "Misc Shrimp", "Misc Brachyura", "Opilio Crab",
               "Pandalidae (shrimp)", "Misc Anomura", "Crangonidae (shrimp)",
               "Hippolytidae (shrimp)", "Paguridae", "Chionoecetes spp.", "Misc Majidae",
               "Misc Lithodidae")
worm_prey <- c("Polychaeta", "Chaetognatha", "Misc Worm")
cephalopod_prey <- c("Octopoda", "Misc Cephalopoda", "Teuthida")
echino_prey <- c("Sea Cucumber", "Sea Urchin", "Misc Echinoderm", "Sand Dollar",
                 "Brittle Star")
pelagic_crustacean <- c("Isopoda", "Euphausiacea", "Copepoda", "Misc Crustacea",
                        "Mysidacea", "Gammaridea", "Hyperiidea", "Capreillidea",
                        "Misc Amphipoda", "Cumacea")


specimen_diet <- raw_diet %>%
  select(prey_name, pred_name) %>%
  filter(!prey_name %in% c("Empty", "Misc", "Misc Org", "Misc Invert", "Offal",
                           "Misc Mollusca")) %>%
  mutate(prey_cat = case_when(
    prey_name %in% fish_prey ~ "fish",
    prey_name %in% decapod_prey ~ "decapod",
    prey_name %in% worm_prey ~ "worm",
    prey_name %in% cephalopod_prey ~ "cephalopod", 
    prey_name %in% echino_prey ~ "echinoderm",
    prey_name %in% pelagic_crustacean ~ "pelagic_crustacean",
    prey_name == "Misc Bird" ~ "bird",
    prey_name %in% c("Larvacea", "Tunicate") ~ "tunicate",
    prey_name %in% c("Gastropod", "Pteropod") ~ "gastropod",
    grepl("Eggs", prey_name) ~ "eggs",
    TRUE ~ prey_name
  )) 



species_diet <- specimen_diet %>%
  group_by(pred_name) %>%
  add_count(prey_cat, name = "prey_prop") %>%
  mutate(prop = (prey_prop/n()) * 100) %>%
  select(species = pred_name, prey_cat, prop) %>%
  unique() %>%
  #mutate(primary_diet = ifelse(any(prop > 50), prey_cat[which.max(prop)], "generalist")) %>%
  pivot_wider(names_from = prey_cat, values_from = prop, values_fill = 0) 



# combining fishbase and ecological data
all_eco_data <- left_join(fb_data, species_diet, by = "species")


write_csv(all_eco_data, here("data/REEM_data/species_ecology.csv"))

# I then manually combined with diet data from fishbase in the excel file for this finalized diet table. Also added data from the sources detailed within the columns of the file

eco_clean <- read_csv(here("data/species_ecology_complete.csv")) %>%
  clean_names() %>%
  select(species:length_cm, 
         migration, diet_cat, feeding_pos:salinity_max,
         parental_care:social) %>%
  filter(fishbase_name %in% gsub("_", " ", fixphy$tip.label))

#need geographic extent too
