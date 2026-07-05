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
