
# Setup ----
anno <- SomaDataIO::getAnalyteInfo(sample.adat)

withr::with_seed(101, {
  df <- data.frame(
    statistic = rnorm(5),
    p.value   = runif(5),
    rank      = 1:5,
    row.names = sample(anno$AptName, 5)    # important; merge by field
  )
})
true <- data.frame(
  row.names = c("seq.4545.53", "seq.2682.68", "seq.5004.69",
                "seq.3115.64", "seq.4801.13"),
  Target = c("DnaJ homolog", "HSP 60", "MK11", "MK01", "PERL"),
  TargetFullName = c("Mitochondrial import inner membrane translocase subunit TIM14",
                     "60 kDa heat shock protein; mitochondrial",
                     "Mitogen-activated protein kinase 11",
                     "Mitogen-activated protein kinase 1",
                     "Lactoperoxidase"),
  EntrezGeneSymbol = c("DNAJC19", "HSPD1", "MAPK11", "MAPK1", "LPO"),
  SomaId = c("SL006197", "SL000450", "SL007453", "SL006918", "SL007153"),
  UniProt = c("Q96DA6", "P10809", "Q15759", "P28482", "P22079"),
  statistic = c(1.48807490395117, 0.22836751437454, -0.758357550130306,
                -1.76704244076856, 1.71510991939593),
  p.value = c(0.661061500199139, 0.923318882007152, 0.795719761401415,
              0.0712125543504953, 0.389407767681405),
  rank = c(1L, 2L, 3L, 4L, 5L)
)


# Testing ----
test_that("`addTargetInfo()` generates the expected joined data frame", {
  x <- addTargetInfo(df, anno)
  expect_equal(x, true)
})

test_that("`addTargetInfo()` generates the expected joined data frame with added meta", {
  x <- addTargetInfo(df, anno, add.fields = c("CalReference", "ColCheck"))
  # add new fields to `true`
  true$CalReference <- c(512.50, 4895.80, 583.85, 2696.10, 1359.50)
  true$ColCheck     <- rep("PASS", 5)
  # reorder
  true <- dplyr::select(true, -statistic, -p.value, -rank, dplyr::everything())
  expect_equal(x, true)
})
