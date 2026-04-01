# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(usethis)
library(readr)

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

# check the distribution of p0
pheno %>%
  pull(p0) %>%
  table()


ped_pheno <- ped %>%
  full_join(pheno, by = c("ID", "dadID", "momID"))

usethis::use_data(guinea_pigs, overwrite = TRUE, compress = "xz")
