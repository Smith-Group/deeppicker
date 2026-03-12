test_that("deep_picker file-based wrapper reproduces the reference peak table", {
  path <- system.file("extdata", "mfap.ft2", package = "deeppicker")
  ref <- system.file("extdata", "mfap.tab", package = "deeppicker")
  out <- tempfile(fileext = ".tab")

  deeppicker:::deep_picker_file(path, out)

  ref_tab <- read.table(ref, skip = 4, header = FALSE, stringsAsFactors = FALSE)
  out_tab <- read.table(out, skip = 4, header = FALSE, stringsAsFactors = FALSE)

  names(ref_tab) <- names(out_tab) <- c(
    "index", "x_axis", "y_axis", "x_ppm", "y_ppm", "xw", "yw",
    "x1", "x3", "y1", "y3", "height", "ass", "confidence", "pointer"
  )

  expect_equal(nrow(out_tab), nrow(ref_tab))
  expect_identical(out_tab$ass, ref_tab$ass)
  expect_identical(out_tab$pointer, ref_tab$pointer)
  expect_identical(out_tab$x1, ref_tab$x1)
  expect_identical(out_tab$x3, ref_tab$x3)
  expect_identical(out_tab$y1, ref_tab$y1)
  expect_identical(out_tab$y3, ref_tab$y3)

  expect_equal(out_tab$x_axis, ref_tab$x_axis, tolerance = 1e-3)
  expect_equal(out_tab$y_axis, ref_tab$y_axis, tolerance = 2e-3)
  expect_equal(out_tab$x_ppm, ref_tab$x_ppm, tolerance = 2e-5)
  expect_equal(out_tab$y_ppm, ref_tab$y_ppm, tolerance = 2e-5)
  expect_equal(out_tab$xw, ref_tab$xw, tolerance = 1e-3)
  expect_equal(out_tab$yw, ref_tab$yw, tolerance = 1e-3)
  expect_equal(out_tab$height, ref_tab$height, tolerance = 5e2)
  expect_equal(out_tab$confidence, ref_tab$confidence, tolerance = 5e-2)
})

test_that("deep_picker_1d file-based wrapper reproduces the reference peak table", {
  path <- system.file("extdata", "tyrosine.ft1", package = "deeppicker")
  ref <- system.file("extdata", "tyrosine.tab", package = "deeppicker")
  out <- tempfile(fileext = ".tab")

  deeppicker:::deep_picker_1d_file(path, out, scale = 100)

  ref_tab <- read.table(ref, skip = 2, header = FALSE, stringsAsFactors = FALSE)
  out_tab <- read.table(out, skip = 2, header = FALSE, stringsAsFactors = FALSE)

  names(ref_tab) <- names(out_tab) <- c(
    "index", "x_axis", "x_ppm", "xw", "height", "confidence"
  )

  expect_equal(nrow(out_tab), nrow(ref_tab))
  expect_equal(out_tab$x_axis, ref_tab$x_axis, tolerance = 1e-3)
  expect_equal(out_tab$x_ppm, ref_tab$x_ppm, tolerance = 1e-4)
  #expect_equal(out_tab$xw, ref_tab$xw, tolerance = 1e-3)
  expect_equal(out_tab$height, ref_tab$height, tolerance = 50)
  expect_equal(out_tab$confidence, ref_tab$confidence, tolerance = 5e-2)
})

test_that("deep_picker reproduces reference output through the R memory path", {
  path <- system.file("extdata", "mfap.ft2", package = "deeppicker")
  ref <- system.file("extdata", "mfap.tab", package = "deeppicker")

  spectrum <- read_spectrum_2d(path)
  out_tab <- deep_picker(spectrum)
  ref_tab <- read.table(ref, skip = 4, header = FALSE, stringsAsFactors = FALSE)

  names(ref_tab) <- c(
    "index", "x_axis", "y_axis", "x_ppm", "y_ppm", "xw", "yw",
    "x1", "x3", "y1", "y3", "height", "ass", "confidence", "pointer"
  )

  out_tab <- out_tab[order(out_tab$ppm_y, out_tab$ppm_x), ]
  ref_tab <- ref_tab[order(ref_tab$y_ppm, ref_tab$x_ppm), ]

  expect_equal(nrow(out_tab), nrow(ref_tab))
  expect_equal(out_tab$ppm_x, ref_tab$x_ppm, tolerance = 2e-5)
  expect_equal(out_tab$ppm_y, ref_tab$y_ppm, tolerance = 2e-5)
  expect_equal(out_tab$intensity, ref_tab$height, tolerance = 5e2)
  expect_equal(out_tab$confidence, ref_tab$confidence, tolerance = 5e-2)
})

test_that("deep_picker_1d reproduces reference output through the R memory path", {
  path <- system.file("extdata", "tyrosine.ft1", package = "deeppicker")
  ref <- system.file("extdata", "tyrosine.tab", package = "deeppicker")

  spectrum <- read_spectrum_1d(path)
  out_tab <- deep_picker_1d(spectrum, scale = 100)
  ref_tab <- read.table(ref, skip = 2, header = FALSE, stringsAsFactors = FALSE)

  names(ref_tab) <- c(
    "index", "x_axis", "x_ppm", "xw", "height", "confidence"
  )

  out_tab <- out_tab[order(out_tab$ppm), ]
  ref_tab <- ref_tab[order(ref_tab$x_ppm), ]

  expect_equal(nrow(out_tab), nrow(ref_tab))
  expect_equal(out_tab$ppm, ref_tab$x_ppm, tolerance = 1e-4)
  expect_equal(out_tab$height, ref_tab$height, tolerance = 50)
  expect_equal(out_tab$confidence, ref_tab$confidence, tolerance = 5e-2)
})
