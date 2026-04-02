# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)

#  https://doi.org/10.6084/m9.figshare.31513204.v2
ped <- read_delim("data-raw/Pedigree4G.csv",
  delim = ";", escape_double = FALSE,
  trim_ws = TRUE
) %>%
  janitor::clean_names() %>%
  rename(
    ID = id,
    dadID = sire,
    momID = dam
  )

pheno <- read_delim("data-raw/FenotiposPesos90.csv",
  col_types = cols(p0 = col_character()),
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) %>%
  janitor::clean_names() %>%
  rename(
    ID = id,
    dadID = sire_id,
    momID = dam_id
  ) %>%
  mutate(
    # Standardize color: lowercase and strip whitespace
    color = tolower(trimws(color)),
    # Recode "ABORTO" births as NA for birth weight
    p0 = as.double(case_when(
      p0 == "ABORTO" ~ NA_character_,
      TRUE ~ p0
    ))
  )

guinea_pigs <- ped %>%
  full_join(pheno, by = c("ID", "dadID", "momID")) %>%
  ped2fam(personID = "ID", famID = "famID")

# check for duplicate IDs
dup_id <- guinea_pigs %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(ID)

if (length(dup_id) > 0) {
  message("Duplicate IDs found: ", paste(dup_id, collapse = ", "))
  guinea_pigs %>%
    filter(ID %in% dup_id) %>%
    arrange(ID)
} else {
  message("No duplicate IDs found.")
}

# checks
guinea_pigs_repaired <- checkSex(guinea_pigs,
  code_male   = "M",
  code_female = "H",
  verbose     = TRUE,
  repair      = TRUE
) %>%
  checkParentIDs(
    addphantoms       = TRUE,
    repair            = TRUE,
    parentswithoutrow = FALSE,
    repairsex         = FALSE
  )

checkIDs(guinea_pigs_repaired, personID = "ID")

checkis_acyclic <- checkPedigreeNetwork(guinea_pigs_repaired,
  personID = "ID",
  momID    = "momID",
  dadID    = "dadID",
  verbose  = TRUE
)
checkis_acyclic

if (FALSE) {
  library(ggplot2)

  growth_long <- guinea_pigs %>%
    filter(!is.na(p0)) %>%
    select(ID, sexo, dadID, momID, p0, p15, p30, p45, p60, p90) %>%
    pivot_longer(
      cols      = starts_with("p"),
      names_to  = "day",
      values_to = "weight"
    ) %>%
    mutate(day = as.numeric(str_remove(day, "p")))

  pl <- ggplot(
    growth_long %>% filter(ID %in% sample(unique(growth_long$ID), 1000)),
    aes(x = day, y = weight, group = ID)
  ) +
    geom_line(alpha = 0.1) +
    geom_jitter(alpha = 0.05, width = 1, height = 0) +
    geom_smooth(method = "loess", se = TRUE, aes(group = sexo, color = sexo)) +
    scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 90)) +
    scale_color_brewer(
      palette = "Set1",
      labels = c("H" = "Female", "M" = "Male")
    ) +
    labs(
      title = "Growth trajectories of individuals over time",
      x     = "Day",
      y     = "Weight (g)",
      color = "Sex"
    ) +
    theme_minimal()
  pl
}

if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(guinea_pigs, here("data-raw", "guinea_pigs.csv"))
  usethis::use_data(guinea_pigs, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}
