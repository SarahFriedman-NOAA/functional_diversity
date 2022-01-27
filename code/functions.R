# finds species synonyms associated with fishbase profile for all species in a vector or tree tips

get_fishbase_name <- function(sp) {
  if (class(sp) %in% "phylo") {
    tax_fb <- load_taxa() %>%
      as_tibble() %>%
      select(Species, Family, Order, SpecCode) %>%
      dplyr::mutate(tree_name = gsub("_", " ", Species))

    x <- tibble(tree_name = sp$tip.label) %>%
      mutate(tree_name = gsub("_", " ", tree_name)) %>%
      left_join(., tax_fb, by = "tree_name") %>%
      mutate(in_fishbase = ifelse(!is.na(Family), tree_name, NA)) %>%
      select(tree_name, in_fishbase)


    if (all(!is.na(x$in_fishbase))) {
      cat("All species names are up to date.")
    } else {
      # get synonyms for species that aren't recognized
      syns <- x %>%
        filter(is.na(in_fishbase)) %>%
        mutate(code = purrr:::map(tree_name, rfishbase:::synonyms)) %>%
        unnest() %>%
        filter(!Status %in% c("misapplied name", "ambiguous synonym")) %>%
        select(tree_name, fishbase_name = Species) %>%
        mutate(fishbase_name = ifelse(is.na(fishbase_name), tree_name, fishbase_name))

      out <- full_join(x, syns, by = "tree_name") %>%
        mutate(new_name = ifelse(is.na(in_fishbase), fishbase_name, tree_name),
               tree_name = gsub(" ", "_", tree_name),
               new_name = gsub(" ", "_", new_name)) %>%
        select(tree_name, new_name)
    }
    return(out)
  } else {
    sp <- gsub("_", " ", sp)
    rfishbase::synonyms(sp) %>%
      filter(!Status %in% c("misapplied name", "ambiguous synonym")) %>%
      mutate(Species = gsub(" ", "_", Species)) %>%
      pluck("Species")
  }
}
