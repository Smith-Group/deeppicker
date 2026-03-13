.ppm_from_dimnames <- function(spectrum) {
  rn <- rownames(spectrum)
  cn <- colnames(spectrum)

  if (is.null(rn) || is.null(cn)) {
    stop("`ppm` is NULL, but `spectrum` does not have both row and column names.")
  }

  row_ppm <- suppressWarnings(as.numeric(rn))
  col_ppm <- suppressWarnings(as.numeric(cn))

  if (anyNA(row_ppm) || anyNA(col_ppm)) {
    stop("`ppm` is NULL, but row and column names must be numeric ppm values.")
  }

  if (length(row_ppm) < 2L || length(col_ppm) < 2L) {
    stop("`ppm` is NULL, but row and column names must contain at least two values each.")
  }

  row_diff <- diff(row_ppm)
  col_diff <- diff(col_ppm)

  if (!(all(row_diff > 0) || all(row_diff < 0))) {
    stop("Row names must be strictly monotone to derive indirect-dimension ppm values.")
  }

  if (!(all(col_diff > 0) || all(col_diff < 0))) {
    stop("Column names must be strictly monotone to derive direct-dimension ppm values.")
  }

  row_step <- (row_ppm[length(row_ppm)] - row_ppm[1]) / (length(row_ppm) - 1L)
  col_step <- (col_ppm[length(col_ppm)] - col_ppm[1]) / (length(col_ppm) - 1L)

  c(col_ppm[1], col_step, row_ppm[1], row_step)
}

#' Run DEEP Picker on a 2D NMR Spectrum
#'
#' DEEP Picker is an artificial neural network based 2D NMR spectral peak
#' picking and deconvolution tool. It predicts every 2D cross-peak locally
#' without taking into account the behavior of spectral data points that are
#' further away. In practice, it provides an excellent starting point for
#' downstream quantitative fitting workflows.
#'
#' This wrapper exposes the DEEP Picker 2D peak-picking engine for spectra that
#' are already available in R memory. The `model` argument follows the
#' DEEP Picker guidance:
#' use `1L` for spectra with about 6 to 20 points per peak (typical for protein
#' spectra) and `2L` for spectra with about 4 to 12 points per peak (typical
#' for metabolomics spectra). When `auto_ppp = TRUE`, DEEP Picker adjusts PPP
#' automatically using cubic spline interpolation; the DEEP Picker
#' documentation recommends leaving this enabled unless there is a specific
#' reason not to.
#'
#' @param spectrum Numeric matrix with the indirect dimension in rows and the
#'   direct dimension in columns.
#' @param ppm Optional numeric vector `c(begin1, step1, begin2, step2)` giving
#'   the ppm origin and increment for the direct and indirect dimensions. If
#'   `NULL`, `deep_picker()` derives these values from numeric column names
#'   (direct dimension) and row names (indirect dimension).
#' @param noise Spectrum noise level. If `NULL`, `deep_picker()` estimates the
#'   noise internally using the same default variance-based procedure as the
#'   DEEP Picker command-line program.
#' @param scale Minimal peak amplitude cutoff, expressed as a multiple of the
#'   noise level. The default is `5.5`.
#' @param scale2 Noise-floor cutoff, expressed as a multiple of the noise
#'   level. Spectral points below this threshold are set to zero before peak
#'   picking. The default is `3.0`.
#' @param scale_negative Minimal negative-peak amplitude cutoff, expressed as a
#'   multiple of the noise level. The default is `scale`.
#' @param scale2_negative Negative-peak noise-floor cutoff, expressed as a
#'   multiple of the noise level. The default is `scale2`.
#' @param model ANN model selection. `1L` corresponds to the broader PPP range
#'   typical for protein spectra; `2L` corresponds to the narrower PPP range
#'   typical for metabolomics spectra.
#' @param auto_ppp Logical; whether to adjust PPP automatically using cubic
#'   spline interpolation.
#' @param t1_noise Logical; whether to remove possible `t1` noise peaks.
#' @param negative Logical; whether to pick negative peaks in addition to
#'   positive peaks.
#' @param debug_flag Integer debug flag forwarded to the DEEP Picker core.
#' @param verbose Logical; whether to print DEEP Picker progress messages.
#' @param as_data_frame Logical; if `TRUE`, return a data frame of peaks. If
#'   `FALSE`, return a list with the peak table, estimated median widths, and
#'   noise level.
#'
#' @return If `as_data_frame = TRUE`, a data frame with one row per picked peak
#'   and the following columns:
#'   \describe{
#'     \item{`x`, `y`}{Peak coordinates in DEEP Picker point units for the
#'       direct (`x`) and indirect (`y`) dimensions.}
#'     \item{`ppm_x`, `ppm_y`}{Peak positions converted to ppm for the direct
#'       and indirect dimensions.}
#'     \item{`intensity`}{Estimated peak intensity.}
#'     \item{`sigmax`, `sigmay`}{Estimated Gaussian width components in the
#'       direct and indirect dimensions.}
#'     \item{`gammax`, `gammay`}{Estimated Lorentzian width components in the
#'       direct and indirect dimensions.}
#'     \item{`confidence`}{Peak confidence score on a 0 to 1 scale. For 2D
#'       peaks, this wrapper reports the smaller of the two axis-specific
#'       confidence values returned by DEEP Picker.}
#'   }
#'
#'   If `as_data_frame = FALSE`, a list with components:
#'   \describe{
#'     \item{`peaks`}{The peak data frame described above.}
#'     \item{`median_width`}{Length-2 numeric vector of median peak widths in
#'       DEEP Picker point units, ordered as direct dimension then indirect
#'       dimension.}
#'     \item{`noise_level`}{Noise level used during picking, either supplied by
#'       the user or estimated internally.}
#'   }
#'
#' @examples
#' path <- system.file("extdata", "mfap.ft2", package = "deeppicker")
#' spectrum <- read_spectrum_2d(path)
#' peaks <- deep_picker(spectrum)
#'
#' ppm_x <- as.numeric(colnames(spectrum))
#' ppm_y <- as.numeric(rownames(spectrum))
#' ix <- order(ppm_x)
#' iy <- order(ppm_y)
#' x_inc <- ppm_x[ix]
#' y_inc <- ppm_y[iy]
#' z <- t(spectrum[iy, ix, drop = FALSE])
#'
#' positive <- spectrum[spectrum > 0]
#' base_level <- 6 * stats::median(abs(positive))
#' max_level <- max(positive)
#' levels <- exp(seq(log(base_level), log(max_level), length.out = 8))
#'
#' contour(x_inc, y_inc, z, levels = levels, drawlabels = FALSE, lwd = 0.5,
#'         xlim = rev(range(x_inc)), ylim = rev(range(y_inc)),
#'         xlab = "1H (ppm)", ylab = "15N (ppm)",
#'         main = "DEEP Picker Example: mfap.ft2")
#'
#' cols <- grDevices::hcl.colors(100, "YlOrRd", rev = TRUE)
#' conf_idx <- pmax(1L, ceiling(peaks$confidence * 99))
#' points(peaks$ppm_x, peaks$ppm_y, pch = 16, cex = 0.35, col = cols[conf_idx])
#' legend("topright",
#'        legend = c("Lower Confidence", "Higher Confidence"),
#'        pch = 16, pt.cex = 0.35, col = cols[c(25, 95)], bty = "n")
#'
#' @references
#' Li, D.-W., Hansen, A. L., Yuan, C., Bruschweiler-Li, L., and Bruschweiler,
#'   R. (2021). DEEP Picker is a Deep Neural Network for Accurate Deconvolution
#'   of Complex Two-Dimensional NMR Spectra. Nature Communications, 12, 5229.
#'   DOI: 10.1038/s41467-021-25496-5.
#' @export
deep_picker <- function(spectrum,
                        ppm = NULL,
                        noise = NULL,
                        scale = 5.5,
                        scale2 = 3.0,
                        scale_negative = scale,
                        scale2_negative = scale2,
                        model = 1L,
                        auto_ppp = TRUE,
                        t1_noise = FALSE,
                        negative = FALSE,
                        debug_flag = 0L,
                        verbose = TRUE,
                        as_data_frame = TRUE) {
  if (!is.matrix(spectrum) || !is.numeric(spectrum)) {
    stop("`spectrum` must be a numeric matrix.")
  }

  if (is.null(ppm)) {
    ppm <- .ppm_from_dimnames(spectrum)
  }

  if (!is.numeric(ppm) || length(ppm) != 4L) {
    stop("`ppm` must be numeric length 4: c(begin1, step1, begin2, step2).")
  }

  if (!is.null(noise) && (!is.numeric(noise) || length(noise) != 1L)) {
    stop("`noise` must be NULL or a numeric scalar.")
  }

  if (!is.numeric(scale_negative) || length(scale_negative) != 1L) {
    stop("`scale_negative` must be a numeric scalar.")
  }

  if (!is.numeric(scale2_negative) || length(scale2_negative) != 1L) {
    stop("`scale2_negative` must be a numeric scalar.")
  }

  .Call(
    "C_deeppicker_pick_matrix",
    spectrum,
    as.double(ppm),
    if (is.null(noise)) NULL else as.double(noise),
    as.double(scale),
    as.double(scale2),
    as.double(scale_negative),
    as.double(scale2_negative),
    as.integer(model),
    as.logical(auto_ppp),
    as.logical(t1_noise),
    as.logical(negative),
    as.integer(debug_flag),
    if (isTRUE(as_data_frame)) "data.frame" else "list",
    as.logical(verbose),
    PACKAGE = "deeppicker"
  )
}

#' Run the File-Based DEEP Picker Path
#'
#' Internal helper that runs the file-backed DEEP Picker path on an
#' input spectrum file and writes the resulting peak table to disk. This is
#' mainly useful for regression testing against the command-line program.
#'
#' @param path Path to an input spectrum file accepted by DEEP Picker.
#' @param out Output path for the written peak table.
#' @param scale,scale2,model,auto_ppp,t1_noise,negative Passed through to the
#'   DEEP Picker implementation.
#' @param verbose Logical; whether to print DEEP Picker progress messages.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
deep_picker_file <- function(path,
                             out,
                             scale = 5.5,
                             scale2 = 3.0,
                             scale_negative = scale,
                             scale2_negative = scale2,
                             model = 1L,
                             auto_ppp = TRUE,
                             t1_noise = FALSE,
                             negative = FALSE,
                             verbose = TRUE) {
  invisible(.Call(
    "C_deeppicker_pick_file",
    normalizePath(path, mustWork = TRUE),
    out,
    as.double(scale),
    as.double(scale2),
    as.double(scale_negative),
    as.double(scale2_negative),
    as.integer(model),
    as.logical(auto_ppp),
    as.logical(t1_noise),
    as.logical(negative),
    as.logical(verbose),
    PACKAGE = "deeppicker"
  ))
}
