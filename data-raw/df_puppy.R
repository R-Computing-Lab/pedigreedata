# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)


source(here("data-raw","helperscripts.R"))


#  https://doi.org/10.6084/m9.figshare.31513204.v2
ped <- readxl::read_xlsx("data-raw/12917_2019_2146_MOESM4_ESM.xlsx",
) %>%
  janitor::clean_names() %>%
  rename(
    personID = id_of_the_individual,
    dadID = id_of_the_sire,
    momID = id_of_the_dam,
    litterID = id_of_the_litter
  )
# check for duplicate IDs
ped_dup_id <- dup_id <- ped %>%
  group_by(personID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(personID)

if (length(dup_id) > 0) {
  message("Duplicate IDs found: ", paste(dup_id, collapse = ", "))
  ped %>%
    filter(personID %in% dup_id) %>%
    arrange(personID)
} else {
  message("No duplicate IDs found.")
}




# checks

puppy_repaired <- recodeSex(ped,
  code_male   = 1,
  code_female = 2
) %>% checkSex(.,
  code_male   = 1,
  code_female = 2,
  verbose     = TRUE,
  repair      = TRUE
) %>% rename(
  personID = ID)


checkParentIDs(
  ped = puppy_repaired,
  addphantoms = F,
  repair = TRUE,
  parentswithoutrow = FALSE,
  repairsex = TRUE
)

checkIDs(puppy_repaired)

checkis_acyclic <- checkPedigreeNetwork(puppy_repaired,
  personID = "personID",
  momID    = "momID",
  dadID    = "dadID",
  verbose  = TRUE
)
checkis_acyclic


#----
if (FALSE) {
  library(ggplot2)

 ggplot(puppy_repaired, aes(x = length_of_gestation_days, y = weight_at_birth_g, group= sex,color=sex)) +
   geom_point(alpha = 0.5, aes(size=litter_size)) +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal() +facet_wrap(~season_of_birth)


 library(data.table)

# Convert to data.table
puppy_repaired_dt <- as.data.table(puppy_repaired)

df_summarizePedigrees <- summarizePedigrees(personID="personID",
                                              puppy_repaired_dt, byr = "year_of_birth", verbose = TRUE)

pedadd <- ped2com(ped = puppy_repaired,component = "additive",
                  personID = "personID",
                  momID = "momID",
                  dadID = "dadID",
                  verbose = TRUE,
                  sparse=F,
                  force_symmetric = FALSE,
                  mz_twins =F,
                  isChild_method = "classic")
diag(pedadd) %>% summary()

}

if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(puppy, here("data-raw", "puppy.csv"))
  usethis::use_data(puppy, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}
