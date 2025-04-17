# === === === === === === === === === === === === === === === === === === ===

# test dis_character

# === === === === === === === === === === === === === === === === === === ===

# create test functions ####

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## create example function for testing errors

test_dis_character_invalid <- function(y){

  ## check inputs with disputeR
  dis_character(y, valid = c("Python", "SQL"), null_valid = FALSE)

  ## modify string
  out <- paste0(y, " rules!")

  ## return output
  return(out)

}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

## create example function that uses dis_character()

test_dis_character_valid1 <- function(y){

  ## check inputs with disputeR
  dis_character(y)

  ## modify string
  out <- paste0(y, " rules!")

  ## return output
  return(out)

}

test_dis_character_valid2 <- function(y){

  ## check inputs with disputeR
  dis_character(y, valid = c("ham", "eggs", "bacon"), scalar = FALSE)

  ## modify string
  out <- paste0(y, " rules!")

  ## return output
  return(out)

}

test_dis_character_valid3 <- function(y){

  ## check inputs with disputeR
  dis_character(y, empty_valid = TRUE)

  ## modify string
  out <- paste0(y, " rules!")

  ## return output
  return(out)

}

# === === === === === === === === === === === === === === === === === === ===

# test errors ####

test_that("dis_character failure - x is not listed as valid", {
  expect_snapshot(test_dis_character_invalid(y = "R"), error = TRUE)
  expect_snapshot(test_dis_character_valid2(y = c("Bacon", "Ham")), error = TRUE)
})

test_that("dis_character failure - x is not a scalar", {
  expect_snapshot(test_dis_character_invalid(y = c("R", "Python")), error = TRUE)
})

test_that("dis_character failure - x is NULL", {
  expect_snapshot(test_dis_character_invalid(y = NULL), error = TRUE)
})

test_that("dis_character failure - x is empty", {
  expect_snapshot(test_dis_character_invalid(y = ""), error = TRUE)
})

test_that("dis_character failure - x is the incorrect class", {
  expect_snapshot(test_dis_character_invalid(y = 1), error = TRUE)
  expect_snapshot(test_dis_character_invalid(y = TRUE), error = TRUE)
})

# === === === === === === === === === === === === === === === === === === ===

# test successful execution ####

test_that("dis_character success", {
  expect_snapshot(test_dis_character_valid1(y = "R"))
  expect_snapshot(test_dis_character_valid2(y = c("bacon", "ham")))
  expect_snapshot(test_dis_character_valid3(y = ""))
})

# === === === === === === === === === === === === === === === === === === ===
