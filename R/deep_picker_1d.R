.ppm_from_names_1d <- function(spectrum) {
  ppm <- suppressWarnings(as.numeric(names(spectrum)))

  if (is.null(names(spectrum))) {
    stop("`ppm` is NULL, but `spectrum` does not have numeric names.")
  }

  if (anyNA(ppm)) {
    stop("`ppm` is NULL, but `names(spectrum)` must be numeric ppm values.")
  }

  if (length(ppm) < 2L) {
    stop("At least two ppm values are required.")
  }

  ppm_diff <- diff(ppm)
  if (!(all(ppm_diff > 0) || all(ppm_diff < 0))) {
    stop("`names(spectrum)` must be strictly monotone ppm values.")
  }

  c(ppm[1], (ppm[length(ppm)] - ppm[1]) / (length(ppm) - 1L), ppm[length(ppm)])
}

.normalize_ppm_1d <- function(spectrum, ppm) {
  n <- length(spectrum)

  if (is.null(ppm)) {
    return(.ppm_from_names_1d(spectrum))
  }

  if (!is.numeric(ppm)) {
    stop("`ppm` must be NULL or numeric.")
  }

  if (length(ppm) == 3L) {
    return(as.double(ppm))
  }

  if (length(ppm) != n) {
    stop("`ppm` must be length 3 or the same length as `spectrum`.")
  }

  ppm_diff <- diff(ppm)
  if (!(all(ppm_diff > 0) || all(ppm_diff < 0))) {
    stop("`ppm` must be strictly monotone.")
  }

  c(ppm[1], (ppm[n] - ppm[1]) / (n - 1L), ppm[n])
}

#' Run DEEP Picker on a 1D NMR Spectrum
#'
#' DEEP Picker uses artificial neural networks for NMR peak picking and
#' deconvolution. This wrapper exposes the 1D DEEP Picker engine for spectra
#' that are already available in R memory.
#'
#' The `model` argument follows the DEEP Picker guidance: use `1L` for broader
#' peaks and `2L` for narrower peaks. When `auto_ppp = TRUE`, DEEP Picker
#' adjusts PPP automatically using cubic spline interpolation. As in the
#' command-line program, automatic PPP adjustment is disabled when
#' `interp_step` is not `1`.
#'
#' @param spectrum Numeric vector containing the 1D spectrum intensities.
#' @param ppm Optional ppm information. Supply either a numeric vector of length
#'   3, `c(begin, step, stop)`, or a numeric vector the same length as
#'   `spectrum`. If `NULL`, `deep_picker_1d()` derives ppm values from
#'   `names(spectrum)`.
#' @param noise Spectrum noise level. If `NULL`, `deep_picker_1d()` estimates
#'   noise internally using the same default variance-based procedure as the
#'   command-line program.
#' @param scale Minimal peak amplitude cutoff, expressed as a multiple of the
#'   noise level. The default is `5.5`.
#' @param scale2 Signal-region cutoff, expressed as a multiple of the noise
#'   level. The default is `3.0`.
#' @param model ANN model selection. `1L` corresponds to the broader PPP range;
#'   `2L` corresponds to the narrower PPP range used by the 1D command-line
#'   tool by default.
#' @param auto_ppp Logical; whether to adjust PPP automatically using cubic
#'   spline interpolation.
#' @param interp_step Numeric interpolation step. `1` means no interpolation.
#'   Values other than `1` suppress automatic PPP adjustment.
#' @param negative Logical; whether to pick negative peaks in addition to
#'   positive peaks.
#'
#' @return A data frame with one row per picked peak and the following columns:
#'   \describe{
#'     \item{`x`}{Peak coordinate in DEEP Picker point units.}
#'     \item{`ppm`}{Peak position in ppm.}
#'     \item{`height`}{Estimated peak height.}
#'     \item{`sigmax`}{Estimated Gaussian width component.}
#'     \item{`gammax`}{Estimated Lorentzian width component.}
#'     \item{`volume`}{Estimated integrated peak volume.}
#'     \item{`confidence`}{Peak confidence score on a 0 to 1 scale.}
#'   }
#'
#' @examples
#' path <- system.file("extdata", "tyrosine.ft1", package = "deeppicker")
#' spectrum <- read_spectrum_1d(path)
#' peaks <- deep_picker_1d(spectrum, scale = 100)
#'
#' ppm <- as.numeric(names(spectrum))
#' plot(ppm, spectrum, type = "l", xlim = c(7.5, -0.5),
#'      xlab = "1H (ppm)", ylab = "Intensity",
#'      main = "DEEP Picker 1D Example: tyrosine.ft1")
#'
#' cols <- grDevices::hcl.colors(100, "YlOrRd", rev = TRUE)
#' conf_idx <- pmax(1L, ceiling(peaks$confidence * 99))
#' points(peaks$ppm, peaks$height, pch = 16, cex = 0.5, col = cols[conf_idx])
#' legend("topright",
#'        legend = c("Lower Confidence", "Higher Confidence"),
#'        pch = 16, pt.cex = 0.5, col = cols[c(25, 95)], bty = "n")
#'
#' centers <- c(6.73, 4.78, 3.36, 2.72, 1.84, -0.05)
#' half_width <- 0.25
#' old_par <- par(mfrow = c(3, 2))
#' for (center in centers) {
#'   xlim <- c(center + half_width, center - half_width)
#'   in_window <- ppm >= min(xlim) & ppm <= max(xlim)
#'   ylim <- range(spectrum[in_window], finite = TRUE)
#'   plot(ppm, spectrum, type = "l", xlim = xlim, ylim = ylim,
#'        xlab = "1H (ppm)", ylab = "Intensity",
#'        main = sprintf("Window at %.2f ppm", center))
#'   points(peaks$ppm, peaks$height, pch = 16, cex = 0.5, col = cols[conf_idx])
#' }
#' par(old_par)
#'
#' @references
#' Li, D.-W., Hansen, A. L., Yuan, C., Bruschweiler-Li, L., and Bruschweiler,
#'   R. (2021). DEEP Picker is a Deep Neural Network for Accurate Deconvolution
#'   of Complex Two-Dimensional NMR Spectra. Nature Communications, 12, 5229.
#'   DOI: 10.1038/s41467-021-25496-5.
#' @export
deep_picker_1d <- function(spectrum,
                           ppm = NULL,
                           noise = NULL,
                           scale = 5.5,
                           scale2 = 3.0,
                           model = 2L,
                           auto_ppp = TRUE,
                           interp_step = 1,
                           negative = FALSE) {
  if (!is.numeric(spectrum) || is.matrix(spectrum)) {
    stop("`spectrum` must be a numeric vector.")
  }

  if (length(spectrum) < 2L) {
    stop("`spectrum` must contain at least two points.")
  }

  ppm <- .normalize_ppm_1d(spectrum, ppm)

  if (!is.null(noise) && (!is.numeric(noise) || length(noise) != 1L)) {
    stop("`noise` must be NULL or a numeric scalar.")
  }

  if (!is.numeric(interp_step) || length(interp_step) != 1L || interp_step <= 0) {
    stop("`interp_step` must be a positive numeric scalar.")
  }

  .Call(
    "C_deeppicker_pick_1d",
    as.double(spectrum),
    as.double(ppm),
    if (is.null(noise)) NULL else as.double(noise),
    as.double(scale),
    as.double(scale2),
    as.integer(model),
    as.logical(auto_ppp),
    as.double(interp_step),
    as.logical(negative),
    PACKAGE = "deeppicker"
  )
}

#' Run the File-Based 1D DEEP Picker Path
#'
#' Internal helper that runs the file-backed 1D DEEP Picker path on an input
#' spectrum file and writes the resulting peak table to disk.
#'
#' @keywords internal
#' @noRd
deep_picker_1d_file <- function(path,
                                out,
                                scale = 5.5,
                                scale2 = 3.0,
                                noise = NULL,
                                model = 2L,
                                auto_ppp = TRUE,
                                interp_step = 1,
                                negative = FALSE) {
  if (!is.null(noise) && (!is.numeric(noise) || length(noise) != 1L)) {
    stop("`noise` must be NULL or a numeric scalar.")
  }

  invisible(.Call(
    "C_deeppicker_pick_1d_file",
    normalizePath(path, mustWork = TRUE),
    out,
    as.double(scale),
    as.double(scale2),
    if (is.null(noise)) 0 else as.double(noise),
    as.integer(model),
    as.logical(auto_ppp),
    as.double(interp_step),
    as.logical(negative),
    PACKAGE = "deeppicker"
  ))
}
