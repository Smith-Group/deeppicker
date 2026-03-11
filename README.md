# deeppicker R Package

`deeppicker` provides an R interface to the DEEP Picker tools for automated
peak picking in one- and two-dimensional NMR spectra. The package wraps the
DEEP Picker engines, exposes file readers for supported spectrum formats, and
returns picked peaks and confidence scores in R-friendly objects.

The underlying DEEP Picker code and algorithms were created by Da-Wei Li and
Rafael Bruschweiler. The R package interface was created by Colin Smith.

## Upstream project

The original DEEP Picker source repository is:

<https://github.com/lidawei1975/deep>

## Package scope

This package is focused on bringing DEEP Picker into R with:

- wrappers for 1D and 2D peak picking
- readers for supported spectrum formats
- outputs designed for direct use in R workflows

## Citation

If you use `deeppicker`, please cite the DEEP Picker publication:

Li, D.-W., Hansen, A. L., Yuan, C., Bruschweiler-Li, L., and Bruschweiler, R.
(2021). DEEP Picker is a Deep Neural Network for Accurate Deconvolution of
Complex Two-Dimensional NMR Spectra. *Nature Communications*, 12, 5229.
<https://doi.org/10.1038/s41467-021-25496-5>
