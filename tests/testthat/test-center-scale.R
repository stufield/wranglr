# Setup -------
data <- withr::with_seed(101,
  data.frame(
    sample_id  = letters[1:10L],
    seq.1212.1 = rnorm(10, mean = 1000, sd = 25),
    seq.2929.5 = rnorm(10, mean = 2000, sd = 25),
    foo        = runif(10),
    row.names  = LETTERS[1:10L]
  ))
cs    <- center_scale(data)
feats <- c("seq.1212.1", "seq.2929.5", "foo")


# Testing -----
test_that("`center_scale()` unit test default feat (all numerics)", {
  expect_s3_class(cs, "cs_trans")
  expect_equal(dim(cs), dim(data))
  expect_equal(names(cs), names(data))
  expect_equal(sum(apply(cs[, feats], 2, mean)), 0)
  expect_equal(sum(apply(cs[, feats], 2, sd)), 3L)
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
                   -1.62357588410858),
    foo = c(-0.03940475, -0.01465367, 1.09723484, -0.39533930,
            -0.18109608, -0.74313126, -1.30734307, 1.63006664,
            1.13563414, -1.18196748)
  )
  expect_equal(cs, true, ignore_attr = TRUE)

  # attributes are correctly attached
  atts <- attributes(cs)
  expect_named(atts, c("names", "row.names", "class",
                       "par_tbl", "center_lgl", "scale_lgl"))
  expect_s3_class(atts$par_tbl, "tbl_df")
  expect_true(.check_par_tbl(atts$par_tbl))
  expect_equal(data.frame(atts$par_tbl),
               data.frame(
                 feature = feats,
                 means   = c(1006.1260023766, 1989.0081268909, 0.446254),
                 sds     = c(14.610062344689, 24.800702373110, 0.2865708)
               )
  )

  # ensure rownames aren't stripped
  expect_equal(rownames(cs), rownames(data))

  # Check against standard 'base' scale() function
  sc <- scale(data[, feats])
  expect_equal(data.frame(cs[, feats]), data.frame(sc))
})


test_that("`center_scale()` unit test when `feats` param is passed (subset)", {
  feats <- getAnalytes(data)
  cs2   <- center_scale(data, feat = feats)
  expect_equal(cs2$foo, data$foo)   # should be unchanged
  atts <- attributes(cs2)
  expect_named(atts, c("names", "row.names", "class",
                       "par_tbl", "center_lgl", "scale_lgl"))
  expect_s3_class(atts$par_tbl, "tbl_df")
  expect_true(.check_par_tbl(atts$par_tbl))
  expect_equal(data.frame(atts$par_tbl),
               data.frame(
                 feature = feats,
                 means   = c(1006.1260023766, 1989.0081268909),
                 sds     = c(14.610062344689, 24.800702373110)
               )
  )
})

test_that("`undo_center_scale()` generates correct values", {
  # check that `undo_center_scale()` reverses the center/scaling; back to `data`
  undo <- undo_center_scale(cs)
  expect_s3_class(undo, class(data))
  expect_equal(undo, data, ignore_attr = TRUE)   # class will differ; `cs_trans`

  # test that `scaled` element removed; cannot double-undo
  expect_error(
    undo_center_scale(undo),
    "Must perform `undo` on previously center/scaled data."
  )
})

test_that("`center_scale()` generates error for other S3 methods", {
  # tibble
  x <- center_scale(tibble::as_tibble(data))
  expect_s3_class(x, "tbl_df")
  expect_equal(attributes(x)$scaled, attributes(cs)$scaled)
  expect_equal(x, data.frame(cs), ignore_attr = TRUE)

  # matrix
  expect_error(
    center_scale(as.matrix(data[, feats])),
    "The `matrix` method needs work. Please try again later."
  )

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
