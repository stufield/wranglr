
# Setup -------
data <- withr::with_seed(101,
  data.frame(
    sample_id  = letters[1:10L],
    seq.1212.1 = rnorm(10, mean = 1000, sd = 25),
    seq.2929.5 = rnorm(10, mean = 2000, sd = 25),
    row.names  = LETTERS[1:10L]
  )) |> dress_adat()
cs   <- center_scale(data)
apts <- getAnalytes(data)


# Testing -----
test_that("`center_scale()` unit test", {
  expect_s3_class(cs, "soma_adat")
  expect_equal(dim(cs), dim(data))
  expect_equal(names(cs), names(data))
  expect_equal(getAnalytes(cs), apts)
  expect_equal(sum(apply(cs[, apts], 2, mean)), 0)
  expect_equal(sum(apply(cs[, apts], 2, sd)), length(getAnalytes(data)))
  true <- data.frame(
    row.names = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
    sample_id = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
    seq.1212.1 = c(-0.977197379628999, 0.526044573089033, -1.57423000209291,
                   -0.0524991531439502, 0.112472350734023, 1.58953153413111,
                   0.639541693501632, -0.612205481016907, 1.14987222264402,
                   -0.801330358217071),
    seq.2929.5 = c(0.973886756009156, -0.358023641538001, 1.88243707874783,
                   -1.0353988713188, 0.204622779130862, 0.248316515051412,
                   -0.41337520385741, 0.502143462226928, -0.381032990343438,
                   -1.62357588410858)
  )
  expect_equal(data.frame(cs), true)

  # attributes are correctly attached
  atts <- attributes(cs)
  expect_named(atts, c("names", "row.names", "Header.Meta",
                       "Col.Meta", "file_specs", "row_meta", "class",
                       "par_tbl", "center_lgl", "scale_lgl"))
  expect_s3_class(atts$par_tbl, "tbl_df")
  expect_true(.check_par_tbl(atts$par_tbl))
  expect_equal(data.frame(atts$par_tbl),
               data.frame(
                 AptName = apts,
                 means   = c(1006.1260023766, 1989.0081268909),
                 sds     = c(14.610062344689, 24.800702373110)
               )
  )

  # ensure rownames aren't stripped
  expect_equal(rownames(cs), rownames(data))

  # Check against standard 'base' scale() function
  sc <- scale(data[, apts])
  expect_equal(data.frame(cs[, apts]), data.frame(sc))
})


test_that("`center_scale()` generates correct values when passing ref.data", {
  set.seed(1)
  idx   <- sample(seq_len(nrow(data)), 5)
  train <- data[ idx, ]
  test  <- data[-idx, ]
  tbl <- tibble::tibble(AptName = getAnalytes(train),
                        means   = unname(colMeans(strip_meta(train))),
                        sds     = unname(apply(strip_meta(train), 2, sd)))
  new  <- center_scale(test, tbl)
  true <- data.frame(
    row.names = c("C", "E", "F", "H", "J"),
    sample_id = c("c", "e", "f", "h", "j"),
    seq.1212.1 = c(-2.25642363050747, -0.178258491267427, 1.64160788787502,
                   -1.07112509846743, -1.30414352409641),
    seq.2929.5 = c(2.88538110205648, 0.60744267509313, 0.666764877850451,
                   1.01138117829675, -1.87466990915822)
  )
  expect_s3_class(new, "soma_adat")
  expect_equal(data.frame(new), true)
  # check atts
  expect_setequal(names(attributes(new)),
                  c("names", "class", "row.names", "par_tbl", "Header.Meta",
                    "Col.Meta", "file_specs", "row_meta",
                    "center_lgl", "scale_lgl"))
  expect_true(.check_par_tbl(attr(new, "par_tbl")))
  expect_equal(data.frame(attributes(new)$par_tbl),
               data.frame(
                 AptName = apts,
                 means   = c(1009.8830148454, 1982.9867943654),
                 sds     = c(11.857973202624, 18.266943737201)
               )
  )

  # processing via `ref.data =` produces same result
  expect_warning(
    new2 <- center_scale(test, ref.data = train),
    "The `ref.data` argument of `center_scale()` is deprecated as of",
    fixed = TRUE
  )
  expect_equal(new, new2)

  # check that `undo_center_scale()` reverses the center/scaling; back to `test`
  undo <- undo_center_scale(new)
  expect_s3_class(undo, "soma_adat")
  expect_equal(undo, test)

  # test that `scaled` element removed; cannot double-undo
  expect_error(
    undo_center_scale(undo),
    "is_center_scaled(data) is not TRUE", fixed = TRUE
  )
})

test_that("`center_scale()` generates correct values for non-soma_adat objects", {
  # data.frame
  x <- center_scale(data.frame(data))
  expect_s3_class(x, "data.frame")
  expect_equal(attributes(x)$scaled, attributes(cs)$scaled)
  expect_equal(x, data.frame(cs), ignore_attr = TRUE)
  # tibble
  x <- center_scale(tibble::as_tibble(data))
  expect_s3_class(x, "tbl_df")
  expect_equal(attributes(x)$scaled, attributes(cs)$scaled)
  expect_equal(x, data.frame(cs), ignore_attr = TRUE)
  # matrix
  y <- center_scale(as.matrix(data[, apts]))   # must pre-select numeric cols
  expect_true(is.matrix(y))                       # for S3 matrix method
  expect_equal(y, as.matrix(cs[, apts]), ignore_attr = TRUE)
  # default
  expect_error(
    center_scale(1:10L),
    "No S3 method could be found for object of class: 'integer'"
  )
  expect_error(
    center_scale(1.5),
    "No S3 method could be found for object of class: 'numeric'"
  )
  expect_error(
    center_scale(letters),
    "No S3 method could be found for object of class: 'character'"
  )
  expect_error(
    center_scale(data, center = FALSE, scale = FALSE),
    "At least 1 of 'center' or 'scale' must be `TRUE`."
  )
})

test_that("`is_center_scaled()` unit test", {
  expect_true(is_center_scaled(cs))
  expect_false(is_center_scaled(data))
})
