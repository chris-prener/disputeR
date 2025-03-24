# dis_character failure - x is not listed as valid

    Code
      test_dis_character_invalid(x = "R")
    Condition
      Error in `test_dis_character_invalid()`:
      ! `x` must be a valid value for the parameter `x`, not `R`.
      i Valid arguments for `x` are: `Python` and `SQL`.

# dis_character failure - x is not a scalar

    Code
      test_dis_character_invalid(x = c("R", "Python"))
    Condition
      Error in `test_dis_character_invalid()`:
      ! `x` must have a length of `1` (i.e. a scalar), not a length of `2`.
      i Provide a <character> scalar for `x`, such as `x = 'value'`.

# dis_character failure - x is NULL

    Code
      test_dis_character_invalid(x = NULL)
    Condition
      Error in `dis_character()`:
      ! `x` must not be `NULL`.
      i Provide a non-`NULL` value for `x`, such as `x = 'value'`.

# dis_character failure - x is empty

    Code
      test_dis_character_invalid(x = "")
    Condition
      Error in `test_dis_character_invalid()`:
      ! `x` must not be an empty <character> value (i.e. `x = ''`).
      i Provide a <character> scalar for `x`, such as `x = 'value'`.

# dis_character success

    Code
      test_dis_character_valid(x = "R")
    Output
      [1] "R rules!"

