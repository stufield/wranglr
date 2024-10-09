
# Setup ----
df1 <- data.frame(a = 1, b = 2, c = 3, row.names = "A")
df2 <- data.frame(a = 4, b = 5, d = 6, row.names = "B")
df3 <- data.frame(a = 7, b = 8, e = 9, row.names = "C")
df  <- list(a = df1, b = df2, c = df3)

# Testing ----
# bind_intersect ----
test_that("`bind_intersect()` returns correct expected object", {
  # if passing as '...'
  true <- data.frame(stringsAsFactors = FALSE,
         row.names = c("A", "B", "C"),
              data = c("data_01", "data_02", "data_03"),
                 a = c(1, 4, 7),
                 b = c(2, 5, 8))
  expect_equal(bind_intersect(df1, df2, df3), true)
  # if passing a list
  true <- data.frame(stringsAsFactors = FALSE,
         row.names = c("A", "B", "C"),
              data = c("a", "b", "c"),
                 a = c(1, 4, 7),
                 b = c(2, 5, 8))
  expect_equal(bind_intersect(df), true)
  # unnamed list same as passing '...'
  expect_equal(bind_intersect(unname(df)), bind_intersect(df1, df2, df3))
})

# bind_union ----
test_that("`bind_union()` returns correct expected object", {
  true <- data.frame(stringsAsFactors = FALSE,
            row.names = c("A", "B", "C"),
                 data = c("data_01", "data_02", "data_03"),
                    a = c(1, 4, 7),
                    b = c(2, 5, 8),
                    c = c(3, NA, NA),
                    d = c(NA, 6, NA),
                    e = c(NA, NA, 9))
  expect_equal(bind_union(df1, df2, df3), true)
  # if passing a list
  true <- data.frame(stringsAsFactors = FALSE,
            row.names = c("A", "B", "C"),
                 data = c("a", "b", "c"),
                    a = c(1, 4, 7),
                    b = c(2, 5, 8),
                    c = c(3, NA, NA),
                    d = c(NA, 6, NA),
                    e = c(NA, NA, 9))
  expect_equal(bind_union(df), true)
  # unnamed list same as passing '...'
  expect_equal(bind_union(unname(df)), bind_union(df1, df2, df3))
})
