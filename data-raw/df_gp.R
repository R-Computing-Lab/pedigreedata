# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)


# helpers

#' Repair parent IDs using child-specific corrections
#'
#' @param df A data frame
#' @param parent_col Parent ID column to modify ("momID" or "dadID")
#' @param child_col Child ID column used to match corrections
#' @param corrections Named character vector.
#'   Names are child IDs, values are corrected parent IDs.
#' @param only_if_mismatch If TRUE, only replace when current value differs
#'
#' @return Data frame with repaired parent IDs
repair_parent_ids <- function(df,
                              parent_col = c("momID", "dadID"),
                              child_col = "ID",
                              corrections,
                              only_if_mismatch = TRUE) {
  parent_col <- rlang::arg_match(parent_col)

  stopifnot(is.data.frame(df))
  stopifnot(child_col %in% names(df))
  stopifnot(parent_col %in% names(df))
  stopifnot(!is.null(names(corrections)))
  stopifnot(is.character(corrections))

  child_vals <- df[[child_col]]
  parent_vals <- df[[parent_col]]

  matched <- child_vals %in% names(corrections)
  replacement_vals <- unname(corrections[child_vals])

  if (only_if_mismatch) {
    matched <- matched & (is.na(parent_vals) | parent_vals != replacement_vals)
  }

  parent_vals[matched] <- replacement_vals[matched]
  df[[parent_col]] <- parent_vals

  df
}

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

# corrections for known mismatches between the pedigree and phenotype datasets, keyed by child ID

mom_corrections <- c(
  "00111.3" = "0011.1",
  "0251.22" = "251",
  "0251.31" = "251",
  "0251.32" = "251",
  "0251.33" = "251",
  "0251.34" = "251",
  "0252.21" = "252",
  "0252.23" = "252",
  "0252.31" = "252",
  "0304.21" = "304",
  "0203.2" = "203",
  "0203.3" = "203",
  "0203.4" = "203",
  "0108.1" = "108",
  "0108.2" = "108",
  "0108.3" = "108",
  "0108.4" = "108",
  "0108.21" = "108",
  "0108.22" = "108",
  "0108.31" = "108",
  "0108.32" = "108",
  "0108.33" = "108",
  "0108.34" = "108",
  "0108.41" = "108",
  "0108.42" = "108",
  "0108.43" = "108",
  "0108.44" = "108",
  "008535322.1" = "00853532.02"
)


#---
# read in the data
#---


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
    momID = str_replace_all(momID, "-", ".")
  ) %>%
  repair_parent_ids(
    parent_col = "momID",
    corrections = mom_corrections
  ) %>%
  mutate(
    ID = case_when(
      ID == "01553422.21" & momID == "0155342.2" ~ "015534202.21",
      ID == "0083233.21" & momID == "008323.3" ~ "00832303.21",
      ID == "00853532.2" & momID == "0085353.2" ~ "00853532.02",
      ID == "00853532.21" & momID == "0085353.2" ~ "008535302.21",
      ID == "00853532.22" & momID == "0085353.2" ~ "008535302.22",
      ID == "01553421.1" & momID == "0155342.1" ~ "015534201.1",
      ID == "01553422.1" & momID == "015534.22" ~ "01553422.01",
      ID == "01553424.3" & momID == "0155342.4" ~ "01553424.03",
      TRUE ~ ID
    )
  )


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
    # Standardize color: lowercase and strip whitespace
    color = tolower(trimws(color)),
    # Recode "ABORTO" births as NA for birth weight
    p0 = as.double(case_when(
      p0 == "ABORTO" ~ NA_character_,
      TRUE ~ p0
    ))
  ) %>%
  repair_parent_ids(
    parent_col = "momID",
    corrections = mom_corrections
  ) %>%
  mutate(
    ID = case_when(
      ID == "01553422.21" & momID == "0155342.2" ~ "015534202.21",
      ID == "0083233.21" & momID == "008323.3" ~ "00832303.21",
      ID == "00853532.2" & momID == "0085353.2" ~ "00853532.02",
      ID == "00853532.21" & momID == "0085353.2" ~ "008535302.21",
      ID == "00853532.22" & momID == "0085353.2" ~ "008535302.22",
      ID == "01553421.1" & momID == "0155342.1" ~ "015534201.1",
      ID == "01553424.3" & momID == "0155342.4" ~ "01553424.03",
      TRUE ~ ID
    )
  )
dup_id <- pheno %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(ID)

# [1] "00853532.2"  "00853532.21" "00853532.22" "01553421.1"  "01553421.2"  "01553421.3"  "01553422.1"  "01553422.22"
# [9] "01553422.23" "01553422.31" "01553422.32" "01553422.33" "01553422.34" "01553424.1"  "01553424.2"  "01553424.21"
# [17] "01553424.22" "01553424.23" "01553424.3"  "02502121.3"
id_check <- dup_id[1]

pheno %>% filter(ID %in% id_check | momID %in% id_check | dadID %in% id_check) -> test_df

# View(test_df)

validate_id_convention(pheno)

# check for duplicate IDs


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


# check if mom and dad id match across the two datasets

ped_check <- left_join(ped, pheno, by = "ID", suffix = c(".ped", ".data")) %>%
  filter(!is.na(dadID.ped) & !is.na(dadID.data) & dadID.ped != dadID.data |
    !is.na(momID.ped) & !is.na(momID.data) & momID.ped != momID.data)

ped_check %>%
  select(ID, sexo, dadID.ped, dadID.data, momID.ped, momID.data) %>%
  print(n = Inf)

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

guinea_pigs_repaired <- recodeSex(guinea_pigs,
  code_male   = "M",
  code_female = "H"
) %>% checkSex(.,
  code_male   = "M",
  code_female = "H",
  verbose     = TRUE,
  repair      = TRUE
)


checkParentIDs(
  addphantoms       = F,
  repair            = TRUE,
  parentswithoutrow = FALSE,
  repairsex         = TRUE
)

checkIDs(guinea_pigs_repaired)

checkis_acyclic <- checkPedigreeNetwork(guinea_pigs_repaired,
  personID = "ID",
  momID    = "momID",
  dadID    = "dadID",
  verbose  = TRUE
)
checkis_acyclic


#----
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
