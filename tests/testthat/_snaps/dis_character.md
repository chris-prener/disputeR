# dis_character failure - x is not listed as valid

    Code
      test_dis_character_invalid(y = "R")
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must be a valid value for the parameter `y`, not `R`.
      i Valid arguments for `y` are: `Python` and `SQL`.

# dis_character failure - x is not a scalar

    Code
      test_dis_character_invalid(y = c("R", "Python"))
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must have a length of `1` (i.e. a scalar), not a length of `2`.
      i Provide a <character> scalar for `y`, such as `y = 'value'`.

# dis_character failure - x is NULL

    Code
      test_dis_character_invalid(y = NULL)
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must not be `NULL`.
      i Provide a non-`NULL` value for `y`, such as `y = 'value'`.

# dis_character failure - x is empty

    Code
      test_dis_character_invalid(y = "")
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must not be an empty <character> value (i.e. `y = ''`).
      i Provide a <character> scalar for `y`, such as `y = 'value'`.

# dis_character failure - x is numeric

    Code
      test_dis_character_invalid(y = 1)
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must be a <character> scalar, not a <numeric> scalar.
      i Provide a <character> scalar for `y`, such as `y = 'value'`..

# dis_character success

    Code
      test_dis_character_valid(y = "R")
    Output
      [1] "R rules!"

