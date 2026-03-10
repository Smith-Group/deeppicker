test_that("read_spectrum_1d returns a named numeric vector", {
  path <- system.file("extdata", "tyrosine.ft1", package = "deeppicker")
  spectrum <- read_spectrum_1d(path)

  expect_type(spectrum, "double")
  expect_null(dim(spectrum))
  expect_length(spectrum, 32768)
  expect_equal(length(names(spectrum)), length(spectrum))
  expect_false(anyNA(suppressWarnings(as.numeric(names(spectrum)))))
})

test_that("read_spectrum_2d returns a dimnamed numeric matrix", {
  path <- system.file("extdata", "mfap.ft2", package = "deeppicker")
  spectrum <- read_spectrum_2d(path)

  expect_true(is.matrix(spectrum))
  expect_equal(dim(spectrum), c(256L, 704L))
  expect_equal(length(rownames(spectrum)), nrow(spectrum))
  expect_equal(length(colnames(spectrum)), ncol(spectrum))
  expect_false(anyNA(suppressWarnings(as.numeric(rownames(spectrum)))))
  expect_false(anyNA(suppressWarnings(as.numeric(colnames(spectrum)))))
})
