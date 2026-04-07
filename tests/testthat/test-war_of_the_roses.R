test_that("war_of_the_roses loads as a data frame", {
  data(war_of_the_roses, package = "pedigreedata")
  expect_s3_class(war_of_the_roses, "data.frame")
})

test_that("war_of_the_roses has expected dimensions", {
  data(war_of_the_roses, package = "pedigreedata")
  expect_gte(nrow(war_of_the_roses), 95)
  expect_gte(ncol(war_of_the_roses), 9)
})

test_that("war_of_the_roses has required columns", {
  data(war_of_the_roses, package = "pedigreedata")
  expect_true(all(
    c("id", "famID", "momID", "dadID", "name", "sex", "url", "twinID", "zygosity") %in%
      names(war_of_the_roses)
  ))
})

test_that("war_of_the_roses id has no NAs", {
  data(war_of_the_roses, package = "pedigreedata")
  expect_false(anyNA(war_of_the_roses$id))
})

test_that("war_of_the_roses famID is assigned", {
  data(war_of_the_roses, package = "pedigreedata")
  expect_false(all(is.na(war_of_the_roses$famID)))
})

test_that("war_of_the_roses sex codes are valid", {
  data(war_of_the_roses, package = "pedigreedata")
  valid_sex <- c("M", "F", "U", NA)
  expect_true(all(war_of_the_roses$sex %in% valid_sex))
})

test_that("war_of_the_roses contains Edward III", {
  data(war_of_the_roses, package = "pedigreedata")
  expect_true("Edward III" %in% war_of_the_roses$name)
})

test_that("war_of_the_roses has founders with both parents NA", {
  data(war_of_the_roses, package = "pedigreedata")
  n_founders <- sum(is.na(war_of_the_roses$momID) & is.na(war_of_the_roses$dadID))
  expect_gt(n_founders, 0)
})

test_that("war_of_the_roses parent IDs refer to known IDs or NA", {
  data(war_of_the_roses, package = "pedigreedata")
  known_ids <- war_of_the_roses$id
  dad_ids <- war_of_the_roses$dadID[!is.na(war_of_the_roses$dadID)]
  mom_ids <- war_of_the_roses$momID[!is.na(war_of_the_roses$momID)]
  expect_true(all(dad_ids %in% known_ids))
  expect_true(all(mom_ids %in% known_ids))
})

test_that("war_of_the_roses twin pairs are symmetric", {
  data(war_of_the_roses, package = "pedigreedata")
  twins <- war_of_the_roses[!is.na(war_of_the_roses$twinID), ]
  if (nrow(twins) > 0) {
    # each twin's co-twin should also list them back
    for (i in seq_len(nrow(twins))) {
      ego_id <- twins$id[i]
      twin_id <- twins$twinID[i]
      cotwin_row <- war_of_the_roses[war_of_the_roses$id == twin_id, ]
      expect_equal(cotwin_row$twinID, ego_id)
    }
  }
})
