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

puppy_repaired <- checkParentIDs(
  ped = puppy_repaired,
  addphantoms = F,
  repair = TRUE,
  parentswithoutrow = T,
  repairsex = TRUE
)

checkIDs(puppy_repaired)

checkis_acyclic <- checkPedigreeNetwork(puppy_repaired,
  personID = "ID",
  momID    = "momID",
  dadID    = "dadID",
  verbose  = TRUE
)
checkis_acyclic


#----
if (T) {





puppy_repaired_skin <- puppy_repaired %>% ped2fam(personID="ID") %>%
 # filter(litterID<155) %>%
  select(personID=ID , momID, dadID, famID, sex, weight_at_birth_g) %>%
  mutate(has_longevity = !is.na(weight_at_birth_g))


minim_family_size <- 3000



# Families with at least two observed longevity values.
# A one-person family cannot contribute covariance information.
usable_fams <- puppy_repaired_skin %>%
  filter(!is.na(weight_at_birth_g)) %>%
  count(famID, name = "n_longevity") %>%
  filter(n_longevity >= minim_family_size) %>%
  pull(famID)

length(usable_fams)

puppy_ped <- puppy_repaired_skin

for (i in seq_along(usable_fams)) {
  fam_i <- subset(puppy_ped, famID == usable_fams[i])

 add_i <- ped2add(fam_i,
    sparse = TRUE,
    famID = "famID", personID = "personID",
    momID = "momID", dadID = "dadID", sex = "sex",
    keep_ids = fam_i$personID
  )

  cn_i <- ped2cn(fam_i,
    sparse = TRUE,
    famID = "famID", personID = "personID",
    momID = "momID", dadID = "dadID", sex = "sex",
    keep_ids = fam_i$personID
  )

  mt_i <- ped2mit(fam_i,
    sparse = TRUE,
    famID = "famID", personID = "personID",
    momID = "momID", dadID = "dadID", sex = "sex",
    keep_ids = fam_i$personID
  )

  id_order_i <- rownames(add_i)

  pheno_vals_i <- fam_i$weight_at_birth_g[match(id_order_i, as.character(fam_i$personID))]

  has_longevity_i <- fam_i$has_longevity[match(id_order_i, as.character(fam_i$personID))]

  names(pheno_vals_i) <- id_order_i
  names(has_longevity_i) <- id_order_i

  observed_i <- has_longevity_i %in% TRUE

raw_obs_ids_i <- id_order_i[observed_i]
obs_ids_i <- mx_safe_id(raw_obs_ids_i)

  add_i_obs <- add_i[raw_obs_ids_i, raw_obs_ids_i]
  cn_i_obs <- cn_i[raw_obs_ids_i, raw_obs_ids_i]
  mt_i_obs <- mt_i[raw_obs_ids_i, raw_obs_ids_i]

  rownames(add_i_obs) <- colnames(add_i_obs) <- obs_ids_i
  rownames(cn_i_obs) <- colnames(cn_i_obs) <- obs_ids_i
  rownames(mt_i_obs) <- colnames(mt_i_obs) <- obs_ids_i

  pheno_row_i <- matrix(
    as.double(pheno_vals_i[observed_i]),
    nrow = 1,
    dimnames = list(NULL, obs_ids_i)
  )

  full_groups[[i]] <- buildOneFamilyGroup(
    group_name  = paste0("family", usable_fams[i]),
    Addmat      = add_i_obs,
    Nucmat      = cn_i_obs,
    Mtdmat      = mt_i_obs,
    full_df_row = pheno_row_i,
    obs_ids     = obs_ids_i
  )
}


start_vars <- list(
  ad2 = 0.3, # additive genetic
  cn2 = 0.1, # common nuclear environment
  ce2 = 0, # common extended (not estimated here)
  mt2 = 0.1, # mitochondrial
  dd2 = 0, # dominance (not estimated here)
  am2 = 0, # A x Mt interaction (not estimated here)
  ee2 = 0.5 # unique environment
)
multi_model <- buildPedigreeMx(
  model_name   = "MultiPedigreeModel",
  vars         = start_vars,
  group_models = full_groups
)
gc()
saveRDS(multi_model, paste0("data/multi_model_n",minim_family_size,".rds"))
library(OpenMx)
fitted_multi_model <- mxRun(full_multi_model)

saveRDS(fitted_full_longevity, paste0("data/fitted_full_longevity_n",minim_family_size,".rds"))


multi_model <- buildPedigreeMx(
  model_name   = "MultiPedigreeModel",
  vars         = start_vars,
  group_models = group_models
)

fitted_multi <- mxRun(multi_model)



}

if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(puppy_ped, here("data-raw", "puppy.csv"))
  usethis::use_data(puppy_ped, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}


