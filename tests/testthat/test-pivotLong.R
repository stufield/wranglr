
# Setup ----
# create test data
rfu_vec <- seq(34567, 45678, length.out = 6)
ex_data <- data.frame(
  subject_id    = c(1234L, 1234L, 1234L, 4321L, 4321L, 4321L),
  ie            = rep(c("IE1", "IE2", "IE3"), 2),
  clin_var1_ie1 = c(93L, 93L, 93L, 56L, 56L, 56L),
  clin_var1_ie2 = c(84L, 84L, 84L, 79L, 79L, 79L),
  clin_var1_ie3 = c(89L, 89L, 89L, 68L, 68L, 68L),
  clin_var2_ie1 = c(15, 15, 15, 77, 77, 77),
  clin_var2_ie2 = c(99, 99, 99, 92, 92, 92),
  clin_var2_ie3 = c(100, 100, 100, 16, 16, 16),
  clin_var3_ie1 = c("N", "N", "N", "N", "N", "N"),
  clin_var3_ie3 = c("N", "N", "N", "N", "N", "N"),
  seq.15529.33  = rfu_vec
)

# The 'truth' data
true_data <- data.frame(
  subject_id   = c(1234L, 1234L, 1234L, 4321L, 4321L, 4321L),
  ie           = rep(c("IE1", "IE2", "IE3"), 2),
  clin_var1    = c(93L, 84L, 89L, 56L, 79L, 68L),
  clin_var2    = c(15, 99, 100, 77, 92, 16),
  clin_var3    = c("N", NA_character_, "N", "N", NA_character_, "N"),
  seq.15529.33 = rfu_vec
)


# Testing defaults ----
test_that("`pivotLong()` provides correct output for the default arguments", {
  expect_equal(pivotLong(ex_data), true_data)
})

# Testing class fidelity ----
test_that("function returns the same object class as 'data' argument input", {
  # 'data.frame' class
  output <- pivotLong(ex_data)
  expect_s3_class(output, "data.frame")

  # 'tibble' class
  # 'output' values tested above
  output <- pivotLong(tibble::as_tibble(ex_data))
  expect_s3_class(output, "tbl_df")

  # 'soma_adat' class; dummy up a `soma_adat`
  output <- pivotLong(dress_adat(ex_data))
  expect_s3_class(output, "soma_adat")

  # rownames modes and dummy atts will differ
  expect_equal(output, addClass(true_data, "soma_adat"), ignore_attr = TRUE)
})

# Testing Non-defaults ----
test_that("`pivotLong()` returns the expected object with non-default arguments", {

  # rename 2 columns +
  # change '_ie' -> '@tp' to test non-default args
  ex_data2 <- dplyr::rename(ex_data, patient_id = "subject_id", tp = "ie")
  names(ex_data2) <- gsub("_ie", "@tp", names(ex_data2))

  # function call with non-default args
  output <- pivotLong(ex_data2, sub.key = "patient_id", clin.sep = "@tp",
                      tp.key = "tp")

  # initialize truth
  true_data <- data.frame(
      patient_id = c(1234L, 1234L, 1234L, 4321L, 4321L, 4321L),
              tp = rep(c("IE1", "IE2", "IE3"), 2),
       clin_var1 = c(93L, 84L, 89L, 56L, 79L, 68L),
       clin_var2 = c(15, 99, 100, 77, 92, 16),
       clin_var3 = c("N", NA_character_, "N", "N", NA_character_, "N"),
    seq.15529.33 = rfu_vec
  )
  expect_equal(output, true_data)
})

# Tests more edges ----
test_that("`pivotLong()` returns the expected object with edge case data", {
  data <- tibble::tibble(
    id         = rep(c("a", "b"), each = 3),
    time       = rep(c("IE1", "IE2", "IE3"), 2),
    `Clin-1@t1` = c(93L, 93L, 93L, 56L, 56L, 56L),
    `Clin-2@t1` = c(1.5, 1.5, 1.5, 7.7, 7.7, 7.7),
    `Clin-2@t2` = c(9.9, 9.9, 9.9, 9.2, 9.2, 9.2),
    `Clin-3@t1` = c("N", "N", "N", "N", "N", "N"),
    `Clin-3@t2` = c("Y", "Y", "Y", "Y", "Y", "Y"),
    `Clin-3@t3` = c("N", "N", "N", "Y", "Y", "Y"),
    seq.15529.33  = rfu_vec
  )
  ret <- pivotLong(data, sub.key = "id", tp.key = "time", clin.sep = "@t")
  expect_s3_class(ret, "tbl_df")
  # tests type-fidelity
  expect_equal(
    vapply(ret, typeof, ""),
    c(id       = "character", time = "character",
      `Clin-1` = "integer", `Clin-2` = "double",
      `Clin-3` = "character", seq.15529.33  = "double")
  )
  true <- tibble::tibble(
      id       = rep(c("a", "b"), each = 3),
      time     = rep(c("IE1", "IE2", "IE3"), 2),
      `Clin-1` = c(93L, NA_integer_, NA_integer_, 56L, NA_integer_, NA_integer_),
      `Clin-2` = c(1.5, 9.9, NA_real_, 7.7, 9.2, NA_real_),
      `Clin-3` = c("N", "Y", "N", "N", "Y", "Y"),
      seq.15529.33 = data$seq.15529.33
  )
  expect_equal(ret, true)
})
