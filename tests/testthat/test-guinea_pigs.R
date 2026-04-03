test_that("guinea_pigs loads as a data frame", {
  data(guinea_pigs, package = "pedigreedata")
  expect_s3_class(guinea_pigs, "data.frame")
})

test_that("guinea_pigs has expected dimensions", {
  data(guinea_pigs, package = "pedigreedata")
  expect_gt(nrow(guinea_pigs), 0)
  expect_gt(ncol(guinea_pigs), 0)
})

test_that("guinea_pigs has required pedigree columns", {
  data(guinea_pigs, package = "pedigreedata")
  expect_true(all(c("ID", "dadID", "momID", "famID") %in% names(guinea_pigs)))
})

test_that("guinea_pigs has required phenotype columns", {
  data(guinea_pigs, package = "pedigreedata")
  expect_true(all(
    c(
      "sexo", "color", "clima", "gener",
      "tc_nac", "tc_destete",
      "p0", "p15", "p30", "p45", "p60", "p90"
    ) %in% names(guinea_pigs)
  ))
})

test_that("guinea_pigs ID column has no NAs", {
  data(guinea_pigs, package = "pedigreedata")
  expect_false(anyNA(guinea_pigs$ID))
})

test_that("guinea_pigs famID is assigned", {
  data(guinea_pigs, package = "pedigreedata")
  expect_false(all(is.na(guinea_pigs$famID)))
})

test_that("guinea_pigs sex codes are valid", {
  data(guinea_pigs, package = "pedigreedata")
  valid_sex <- c("M", "H", NA)
  expect_true(all(guinea_pigs$sexo %in% valid_sex))
})

test_that("guinea_pigs generations are in expected range", {
  data(guinea_pigs, package = "pedigreedata")
  gens <- guinea_pigs$gener[!is.na(guinea_pigs$gener)]
  expect_true(all(gens %in% 1:4))
})

test_that("guinea_pigs body weights are positive where recorded", {
  data(guinea_pigs, package = "pedigreedata")
  for (col in c("p0", "p15", "p30", "p45", "p60", "p90")) {
    vals <- guinea_pigs[[col]][!is.na(guinea_pigs[[col]])]
    expect_true(all(vals > 0), label = paste(col, "has non-positive weights"))
  }
})

test_that("guinea_pigs has founders with both parents NA", {
  data(guinea_pigs, package = "pedigreedata")
  n_founders <- sum(is.na(guinea_pigs$dadID) & is.na(guinea_pigs$momID))
  expect_gt(n_founders, 0)
})

test_that("guinea_pigs parent IDs refer to known IDs or NA", {
  data(guinea_pigs, package = "pedigreedata")
  known_ids <- guinea_pigs$ID
  dad_ids <- guinea_pigs$dadID[!is.na(guinea_pigs$dadID)]
  mom_ids <- guinea_pigs$momID[!is.na(guinea_pigs$momID)]
  expect_true(all(dad_ids %in% known_ids))
  expect_true(all(mom_ids %in% known_ids))
})
