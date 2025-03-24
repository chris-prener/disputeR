# === === === === === === === === === === === === === === === === === === ===

# test dis_character

# === === === === === === === === === === === === === === === === === === ===

# create test functions ####

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## create example function for testing errors

test_dis_character_invalid <- function(x){

  ## check inputs with disputeR
  dis_character(x, valid = c("Python", "SQL"), null_valid = FALSE)

  ## modify string
  out <- paste0(x, " rules!")

  ## return output
  return(out)

}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## create example function that uses dis_character()

test_dis_character_valid <- function(x){

  ## check inputs with disputeR
  dis_character(x)

  ## modify string
  out <- paste0(x, " rules!")

  ## return output
  return(out)

}

# === === === === === === === === === === === === === === === === === === ===

# test errors ####

test_that("dis_character failure - x is not listed as valid", {
  expect_snapshot(test_dis_character_invalid(x = "R"), error = TRUE)
})

test_that("dis_character failure - x is not a scalar", {
  expect_snapshot(test_dis_character_invalid(x = c("R", "Python")), error = TRUE)
})

test_that("dis_character failure - x is NULL", {
  expect_snapshot(test_dis_character_invalid(x = NULL), error = TRUE)
})

test_that("dis_character failure - x is empty", {
  expect_snapshot(test_dis_character_invalid(x = ""), error = TRUE)
})

# === === === === === === === === === === === === === === === === === === ===

# test successful execution ####

test_that("dis_character success", {
  expect_snapshot(test_dis_character_valid(x = "R"))
})

# === === === === === === === === === === === === === === === === === === ===
