# dis_character failure - x is not listed as valid

    Code
      test_dis_character_invalid(y = "R")
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must be a valid value for the parameter `y`, not `R`.
      i Valid arguments for `y` are: `Python` and `SQL`.

---

    Code
      test_dis_character_valid2(y = c("Bacon", "Ham"))
    Condition
      Error in `test_dis_character_valid2()`:
      ! `y` must be a valid value for the parameter `y`, not `Bacon` and `Ham`.
      i Valid arguments for `y` are: `ham`, `eggs`, and `bacon`.

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

# dis_character failure - x is the incorrect class

    Code
      test_dis_character_invalid(y = 1)
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must be a <character> scalar, not a <numeric> scalar.
      i Provide a <character> scalar for `y`, such as `y = 'value'`..

---

    Code
      test_dis_character_invalid(y = TRUE)
    Condition
      Error in `test_dis_character_invalid()`:
      ! `y` must be a <character> scalar, not a <logical> scalar.
      i Provide a <character> scalar for `y`, such as `y = 'value'`..

# dis_character success

    Code
      test_dis_character_valid1(y = "R")
    Output
      [1] "R rules!"

---

    Code
      test_dis_character_valid2(y = c("bacon", "ham"))
    Output
      [1] "bacon rules!" "ham rules!"  

---

    Code
      test_dis_character_valid3(y = "")
    Output
      [1] " rules!"

