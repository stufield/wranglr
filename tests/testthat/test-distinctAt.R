
# Setup ----
withr::local_options(c(stringsAsFactors = FALSE))

set.seed(111)
df <- tibble::tibble(
  a_ie = sample(letters[1:3], 20, replace = TRUE),
  b_ie = sample(1:3, 20, replace = TRUE),
  c    = runif(20)
)

tbl_data <- distinctAt(df, a_ie, b_ie)

# Testing ----
test_that("distinctAt provides correct output", {
  expect_s3_class(tbl_data, "tbl_df")
  expect_equal(dim(tbl_data), c(8, 3))
  expect_equal(data.frame(tbl_data),
               data.frame(a_ie = c("b", "c", "c", "a", "a", "c", "b", "b"),
                          b_ie = c(3L, 1L, 3L, 1L, 2L, 2L, 2L, 1L),
                             c = c(0.548273369902745,
                                   0.57583293877542,
                                   0.456315206596628,
                                   0.805540175177157,
                                   0.919282081536949,
                                   0.011725815013051,
                                   0.300707698334008,
                                   0.877583946101367)
                           )
   )
})

test_that("data frame returns a data.frame", {
  df_data  <- distinctAt(data.frame(df), a_ie, b_ie)
  expect_equal(data.frame(tbl_data), df_data)
})

test_that("dplyr verbs functionality works", {
  endsw_data <- distinctAt(df, dplyr::ends_with("ie"))
  expect_equal(tbl_data, endsw_data)
})

test_that("input is equal to output of previous call", {
  new_data <- distinctAt(tbl_data, a_ie, b_ie)
  expect_equal(tbl_data, new_data)
})
