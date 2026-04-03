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
  ) %>%
  mutate(
    ID = str_replace_all(ID, "-", "."),
    dadID = str_replace_all(dadID, "-", "."),
    momID = str_replace_all(momID, "-", "."),
    momID = case_when(
      ID == "00111.3" & momID == "0011..1" ~ "0011.1",
      TRUE ~ momID
    )
  )

ped %>%
  filter(str_detect(ID, "M")) %>%
  select(ID, dadID, momID) %>%
  print(n = Inf)


# check for duplicate IDs
dup_id <- ped %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(ID)

if (length(dup_id) > 0) {
  message("Duplicate IDs found: ", paste(dup_id, collapse = ", "))
  ped %>%
    filter(ID %in% dup_id) %>%
    arrange(ID)
} else {
  message("No duplicate IDs found.")
}

### phenotype

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
    ID = str_replace_all(ID, "-", "."),
    dadID = str_replace_all(dadID, "-", "."),
    momID = str_replace_all(momID, "-", "."),
    momID = case_when(
      ID == "00111.3" & momID == "0011..1" ~ "0011.1",
      TRUE ~ momID
    ),
    # Standardize color: lowercase and strip whitespace
    color = tolower(trimws(color)),
    # Recode "ABORTO" births as NA for birth weight
    p0 = as.double(case_when(
      p0 == "ABORTO" ~ NA_character_,
      TRUE ~ p0
    ))
  )

pheno %>%
  filter(str_detect(dadID, "H")) %>%
  select(ID, dadID, momID) %>%
  print(n = Inf)


# observation -- for the children of founder fathers, the offspring ID is the
# mother's id with the suffix .1, .2, etc. for the different offspring.
# The id is transformed so if it were 123.34 in the mom, that child's id is
# 12334.1, 12334.2, etc.
# The naming still holds for the children of non-founder fathers. It follows the
# mother's id with the suffix .1, .2, etc. for the different offspring.

#' Validate child IDs against the maternal naming convention
#'
#' Convention: child ID = remove_dots(momID) + ".suffix"
#' Allows for leading-zero padding (e.g., mom "11" -> child "0011.1")
#'
#' @param df A data frame with columns ID and momID
#' @param verbose If TRUE, print details of any violations
#' @return Logical: TRUE if all IDs follow the convention, FALSE otherwise
validate_id_convention <- function(df, verbose = TRUE) {
  violations <- df %>%
    filter(!is.na(momID)) %>%
    mutate(
      mom_base = str_replace_all(momID, "\\.", ""),
      match_exact = str_starts(ID, paste0(mom_base, "\\.")),
      match_pad1 = str_starts(ID, paste0("0", mom_base, "\\.")),
      match_pad2 = str_starts(ID, paste0("00", mom_base, "\\.")),
      convention_ok = match_exact | match_pad1 | match_pad2
    ) %>%
    filter(!convention_ok)

  n_checked <- sum(!is.na(df$momID))
  n_violations <- nrow(violations)

  if (n_violations > 0) {
    if (verbose) {
      message(
        "ID convention violations: ", n_violations,
        " of ", n_checked, " non-founder records"
      )
      print(
        violations %>% select(ID, dadID, momID, mom_base),
        n = Inf
      )
    }
    return(FALSE)
  }

  if (verbose) message("All ", n_checked, " child IDs follow the maternal naming convention.")
  TRUE
}

validate_id_convention(pheno)

# check for duplicate IDs
dup_id <- pheno %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(ID)

if (length(dup_id) > 0) {
  message("Duplicate IDs found: ", paste(dup_id, collapse = ", "))
  pheno %>%
    filter(ID %in% dup_id) %>%
    select(ID, dadID, momID, sexo, p0, p15, p30, p45, p60, p90) %>%
    arrange(ID) %>%
    print(n = Inf)
} else {
  message("No duplicate IDs found.")
}
# se if removing the - and replacing it with a . helps

pheno <- pheno %>%
  mutate(
    ID = str_replace_all(ID, "-", "."),
    dadID = str_replace_all(dadID, "-", "."),
    momID = str_replace_all(momID, "-", ".")
  )


guinea_pigs <- ped %>%
  full_join(pheno, by = c("ID", "dadID", "momID")) %>%
  rename(sex = sexo) %>%
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
    addphantoms       = F,
    repair            = TRUE,
    parentswithoutrow = FALSE,
    repairsex         = FALSE
  )

checkIDs(guinea_pigs_repaired)

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
