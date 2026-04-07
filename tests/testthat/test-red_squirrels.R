test_that("red_squirrels loads as a data frame", {
  data(red_squirrels, package = "pedigreedata")
  expect_s3_class(red_squirrels, "data.frame")
})

test_that("red_squirrels has expected dimensions", {
  data(red_squirrels, package = "pedigreedata")
  expect_gte(nrow(red_squirrels), 7799)
  expect_gte(ncol(red_squirrels), 16)
})

test_that("red_squirrels has required pedigree columns", {
  data(red_squirrels, package = "pedigreedata")
  expect_true(all(c("personID", "momID", "dadID", "sex", "famID") %in% names(red_squirrels)))
})

test_that("red_squirrels has required fitness columns", {
  data(red_squirrels, package = "pedigreedata")
  expect_true(all(
    c(
      "byear", "dyear", "lrs", "ars_mean", "ars_max", "ars_med",
      "ars_min", "ars_sd", "ars_n", "year_first", "year_last"
    ) %in% names(red_squirrels)
  ))
})

test_that("red_squirrels personID has no NAs", {
  data(red_squirrels, package = "pedigreedata")
  expect_false(anyNA(red_squirrels$personID))
})

test_that("red_squirrels famID is assigned", {
  data(red_squirrels, package = "pedigreedata")
  expect_false(all(is.na(red_squirrels$famID)))
})

test_that("red_squirrels sex codes are valid", {
  data(red_squirrels, package = "pedigreedata")
  valid_sex <- c("M", "F", NA)
  expect_true(all(red_squirrels$sex %in% valid_sex))
})

test_that("red_squirrels LRS is non-negative where recorded", {
  data(red_squirrels, package = "pedigreedata")
  lrs <- red_squirrels$lrs[!is.na(red_squirrels$lrs)]
  expect_true(all(lrs >= 0))
})

test_that("red_squirrels ARS values are non-negative where recorded", {
  data(red_squirrels, package = "pedigreedata")
  for (col in c("ars_mean", "ars_max", "ars_min")) {
    vals <- red_squirrels[[col]][!is.na(red_squirrels[[col]])]
    expect_true(all(vals >= 0), label = paste(col, "has negative ARS"))
  }
})

test_that("red_squirrels birth year is plausible", {
  data(red_squirrels, package = "pedigreedata")
  byears <- red_squirrels$byear[!is.na(red_squirrels$byear)]
  expect_true(all(byears >= 1987))
  expect_true(all(byears <= 2025))
})

test_that("red_squirrels death year >= birth year where both known", {
  data(red_squirrels, package = "pedigreedata")
  both <- red_squirrels[!is.na(red_squirrels$byear) & !is.na(red_squirrels$dyear), ]
  expect_true(all(both$dyear >= both$byear))
})

test_that("red_squirrels parent IDs refer to known IDs or NA", {
  data(red_squirrels, package = "pedigreedata")
  known_ids <- red_squirrels$personID
  dad_ids <- red_squirrels$dadID[!is.na(red_squirrels$dadID)]
  mom_ids <- red_squirrels$momID[!is.na(red_squirrels$momID)]
  expect_true(all(dad_ids %in% known_ids))
  expect_true(all(mom_ids %in% known_ids))
})
