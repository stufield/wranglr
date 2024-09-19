
dom_example <- list(A = c(5785.1, 5005.6, 5686.3, 5990.8,
                          5235.4, 5340.6, 5272.6, 5905.2),
                    B = c(5708.0, 5300.7, 5339.8, 5393.0,
                          5762.0, 5553.4, 6081.4, 5473.5),
                    C = c(5409.4, 5353.6, 5398.0, 5631.2,
                          5646.1, 5073.4, 5879.2, 5617.5))

test_that("CVdecomp returns the correct values for the Dom set example", {
  cv <- calcCVdecomp(dom_example)
  expect_type(cv, "double")
  expect_equal(cv, c(Intra = 0.050656603606919,
                     Inter = 0.00576366233711441,
                     Total = 0.0509834413562364))
})
