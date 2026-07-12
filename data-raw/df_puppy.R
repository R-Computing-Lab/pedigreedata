# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)
# -----------------------------------------------------------------------------
# Package setup
# -----------------------------------------------------------------------------


library(BGmisc)
library(OpenMx)
library(mvtnorm)
library(tidyverse)
set.seed(202601)
save_runs <- TRUE


true_beta <- list(
  a  = c(1, 0.1, 0.00, 0.00),
  cn = c(0.00, 0.00, 0.00, 0.00),
  ce = c(0.00, 0.00, 0.00, 0.00),
  mt = c(0.00, 0.00, 0.00, 0.00),
  e  = c(1, -0.10, 0.00, 0.00)
)


true_gamma <- list(
  a  = 0.0,
  cn = 0.00,
  ce = 0.00,
  mt = 0.00,
  e  = 0.00
)

required_packages <- c("BGmisc", "OpenMx", "mvtnorm")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}


source(here("data-raw","helperscripts.R"))
source("E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc/data-raw/smoketest_helpers.R")

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


if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(puppy_repaired, here("data-raw", "puppy.csv"))
  usethis::use_data(puppy_ped, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}





puppy_skinny <- puppy_repaired %>% ped2fam(personID="ID") %>%
 # filter(litterID<155) %>%
  select(personID=ID , momID, dadID, famID, sex, weight_at_birth_g, birthYear=year_of_birth,
         litterID
         ) %>%
  mutate( birth_year_scaled = scale(birthYear)) %>%
  checkParentIDs(repair = TRUE, parentswithoutrow = TRUE)

fit_components <- c("a", "e")


# try fitting the family of england data with the temporal AE model
ped_pup <- ped2fam(
  puppy_skinny,
  personID = "ID",
  momID = "momID",
  dadID = "dadID"
) %>%
  ped2maternal() %>%
  ped2paternal() %>%
  rename(personID = ID) %>%
  mutate(personID = as.character(personID),
         momID = as.character(momID),
         dadID = as.character(dadID)
  )

remove(puppy_skinny)



minim_family_size <- 1
maxi_family_size <- 9000


# Families with at least two observed longevity values.
# A one-person family cannot contribute covariance information.
usable_fams <- ped_pup %>%
  filter(is.na(weight_at_birth_g) == FALSE) %>%
  count(famID, name = "n_pheno_yes") %>%
  filter(n_pheno_yes >= minim_family_size &
           n_pheno_yes <= maxi_family_size
         ) %>%
  pull(famID)

length(usable_fams)
n_families <- length(usable_fams)


# scale birth year for use in temporal model
year_mean   <- mean(ped_pup$birthYear, na.rm = TRUE)
year_sd <- sd(ped_pup$birthYear, na.rm = TRUE)
print(paste0("Mean: ", year_mean," SD: ",year_sd))

families <- vector("list", n_families)
group_models <- vector("list", length(usable_fams))

for (i in seq_along(usable_fams)) {
families[[i]] <-   fam_i <- subset(ped_pup, famID == usable_fams[i]) %>%
  mutate(post_mean= ifelse(birthYear >= year_mean, 1, 0))


# Build family group models for temporal A + E.
  group_models[[i]] <- buildOneTemporalFamilyGroup(
    group_name = paste0("family", i),
    Addmat = ped2add(fam_i,  repair_rowless_parents = TRUE,
                     keep_ids = fam_i$personID[!is.na(fam_i$weight_at_birth_g)]),
    Nucmat = NULL,
    Extmat = NULL,
    Mtdmat = NULL,
    Dmgmat = NULL,
    full_df_row = fam_i$weight_at_birth_g[!is.na(fam_i$weight_at_birth_g)],
    obs_ids = fam_i$personID[!is.na(fam_i$weight_at_birth_g)],
    birth_year = fam_i$birth_year_scaled[!is.na(fam_i$weight_at_birth_g)],
    H = fam_i$post_mean[!is.na(fam_i$weight_at_birth_g)] %>% as.matrix(),
    use_exp_loadings = TRUE,
    clean_ids=TRUE
  )
}



# nees to add H, y and birth_year_scaled to the family data frame for plotting and analysis
family_peds <- families %>%
  purrr::map_dfr(~{
    fam <- .
    data.frame(
      fam = fam$famID[!is.na(fam$weight_at_birth_g)],
      y = fam$weight_at_birth_g[!is.na(fam$weight_at_birth_g)],
      birth_year_scaled = fam$birth_year_scaled[!is.na(fam$weight_at_birth_g)],
      post_mean = fam$post_mean[!is.na(fam$weight_at_birth_g)],
      sex = fam$sex[!is.na(fam$weight_at_birth_g)],
      litterID = fam$litterID[!is.na(fam$weight_at_birth_g)]
    )
  })

ggplot2::ggplot(family_peds) +
  ggplot2::geom_point(ggplot2::aes(x = birth_year_scaled, y = y, color = litterID, shape = sex)) +
#  ggplot2::facet_wrap(~fam) +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Phenotypes over time", x = "Scaled Birth Year", y = "Phenotype (y)")




# Build family group models for temporal A + E.




# Parent model with all AE temporal terms present.
temporal_model_ae <- buildTemporalPedigreeMx(
  model_name = "TemporalPedigreeSmokeTest_AE",
  group_models = group_models,
  p_hist = 1,
  components = fit_components,
  ci = FALSE
)

# Stage 1: intercept-only AE.
temporal_model_ae0 <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_e_0", "mean_y")
)
fit_ae0 <- run_and_report(temporal_model_ae0, "AE intercept-only", tries = 20)
if(save_runs){
    gc()
saveRDS(fit_ae0, file = "fit_ae0.rds")
  gc()
}
# Stage 2: AE with linear birth-cohort moderation.
temporal_model_ae_linear <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_a_1", "b_e_0", "b_e_1", "mean_y")
)
fit_ae_linear <- run_and_report(temporal_model_ae_linear, "AE linear time", tries = 30)

if(save_runs){
    gc()
saveRDS(fit_ae_linear, file = "fit_ae_linear.rds")
    gc()
}
# Stage 3: AE with linear birth-cohort moderation plus one historical moderator.
temporal_model_ae_linear_h <- free_only(
  temporal_model_ae,
  labels_to_free = c("b_a_0", "b_a_1", "g_a_1", "b_e_0", "b_e_1", "g_e_1", "mean_y")
)
fit_ae_linear_h <- run_and_report(temporal_model_ae_linear_h, "AE linear time + historical moderator", tries = 30)

if(save_runs){
      gc()
saveRDS(fit_ae_linear_h, file = "fit_ae_linear_h.rds")
    gc()
}
cat("\nTemporal BGmisc-style AE smoke test completed successfully.\n")

target <- c(
  b_a_0 = log(true_beta$a[1]),
  b_a_1 = true_beta$a[2] / true_beta$a[1],
  g_a_1 = true_gamma$a[1] / true_beta$a[1],
  b_e_0 = log(true_beta$e[1]),
  b_e_1 = true_beta$e[2] / true_beta$e[1],
  g_e_1 = true_gamma$e[1]/ true_beta$e[1]
)

est <- omxGetParameters(fit_ae_linear_h)[names(target)]

round(cbind(target = target, estimate = est, diff = est - target), 3)

# graph estimates of a as a function of time and historical moderator



graphing_data <- data.frame(
  time = seq(-3, 3, length.out = 100),
  historical = c(0, 1))

graphing_data$estimated_a_variance <- exp(est["b_a_0"] + est["b_a_1"] * graphing_data$time + est["g_a_1"] * graphing_data$historical)
graphing_data$true_a_variance <- exp(target["b_a_0"] + target["b_a_1"] * graphing_data$time + target["g_a_1"] * graphing_data$historical)
graphing_data$estimated_e_variance <- exp(est["b_e_0"] + est["b_e_1"] * graphing_data$time + est["g_e_1"] * graphing_data$historical)
graphing_data$true_e_variance <- exp(target["b_e_0"] + target["b_e_1"] * graphing_data$time + target["g_e_1"] * graphing_data$historical)
graphing_data$estimated_total_variance <- graphing_data$estimated_a_variance + graphing_data$estimated_e_variance
graphing_data$true_total_variance <- graphing_data$true_a_variance + graphing_data$true_e_variance
graphing_data$unscaled_time <- graphing_data$time * sd(unlist(lapply(families, function(x) x$birth_year_scaled))) + mean(unlist(lapply(families, function(x) x$birth_year_scaled)))

graphing_data_long <- # have a true and estimated factor
graphing_data %>%
  tidyr::pivot_longer(cols = c(estimated_a_variance, true_a_variance, estimated_e_variance, true_e_variance, estimated_total_variance, true_total_variance),
                      names_to = c("type", "component", NA),
                      names_sep = "_",
                      values_to = "variance")


ggplot2::ggplot(graphing_data_long) +
  ggplot2::geom_line(ggplot2::aes(x = unscaled_time, y = variance, linetype = factor(historical),
                                  color = factor(component)
                                  )) +
  ggplot2::labs(title = "Estimated Variance as a function of time and historical moderator", x = "Scaled Birth Year", y = "Estimated Variance", color = "Variance Component") +
  ggplot2::theme_bw() + facet_wrap(~type)




# -----------------------------------------------------------------------------
# Optional AME test after AE runs
# -----------------------------------------------------------------------------

run_optional_ame <- F

if (run_optional_ame) {
  ame_group_models <- vector("list", n_families)
  for (i in seq_len(n_families)) {
    fam <- families[[i]]
    ame_group_models[[i]] <- buildOneTemporalFamilyGroup(
      group_name = paste0("ame_family", i),
      Addmat = fam$A,
      Nucmat = NULL,
      Extmat = NULL,
      Mtdmat = fam$Mt,
      Dmgmat = NULL,
      full_df_row = fam$y,
      obs_ids = fam$obs_ids,
      birth_year = fam$birth_year_scaled,
      H = fam$H,
      use_exp_loadings = TRUE
    )
  }

  temporal_model_ame <- buildTemporalPedigreeMx(
    model_name = "TemporalPedigreeSmokeTest_AME",
    group_models = ame_group_models,
    p_hist = 1,
    components = c("a", "mt", "e"),
    ci = FALSE
  )

  temporal_model_ame0 <- free_only(
    temporal_model_ame,
    labels_to_free = c("b_a_0", "b_mt_0", "b_e_0", "mean_y")
  )
  fit_ame0 <- run_and_report(temporal_model_ame0, "AME intercept-only", tries = 30)
  if(save_runs) {
    gc()
    saveRDS(fit_ame0, file = "fit_ame0.rds")
    gc()
  }
}




