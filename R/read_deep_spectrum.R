#' Read a 1D Spectrum with DEEP Picker Parsers
#'
#' Reads a 1D spectrum file using the file parsers bundled with DEEP Picker and
#' returns the real spectrum as a named numeric vector. The names are the ppm
#' values parsed from the file.
#'
#' Supported formats are chosen by file extension and include NMRPipe
#' (`.ft1`), text (`.txt`), CSV (`.csv`), JSON (`.json`), Sparky (`.ucsf`),
#' and the DEEP Picker `.ldw` format.
#'
#' @param path Path to a 1D spectrum file.
#'
#' @return A named numeric vector containing the real spectrum. The names are
#'   numeric ppm values stored as character strings.
#'
#' @examples
#' path <- system.file("extdata", "tyrosine.ft1", package = "deeppicker")
#' x <- read_spectrum_1d(path)
#' str(x)
#' @export
read_spectrum_1d <- function(path) {
  .Call(
    "C_deeppicker_read_1d",
    normalizePath(path, mustWork = TRUE),
    PACKAGE = "deeppicker"
  )
}

#' Read a 2D Spectrum with DEEP Picker Parsers
#'
#' Reads a 2D spectrum file using the file parsers bundled with DEEP Picker and
#' returns the real spectrum as a numeric matrix. Row names and column names are
#' the ppm values parsed from the file for the indirect and direct dimensions.
#'
#' Supported formats are chosen by file extension and include NMRPipe
#' (`.ft2`), text (`.txt`), CSV (`.csv`), JSON (`.json`), Sparky (`.ucsf`),
#' and the DEEP Picker `.ldw` format.
#'
#' @param path Path to a 2D spectrum file.
#'
#' @return A numeric matrix with row names and column names set to ppm values.
#'
#' @examples
#' path <- system.file("extdata", "mfap.ft2", package = "deeppicker")
#' x <- read_spectrum_2d(path)
#' str(x)
#' @export
read_spectrum_2d <- function(path) {
  .Call(
    "C_deeppicker_read_2d",
    normalizePath(path, mustWork = TRUE),
    PACKAGE = "deeppicker"
  )
}
