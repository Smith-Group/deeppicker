#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

#include "deep/spectrum_pick_1d.h"
#include "deep/spectrum_pick.h"

using namespace Rcpp;

class DeepPickerAdapter : public spectrum_pick
{
public:
    bool load_from_matrix(const NumericMatrix &spectrum,
                          const NumericVector &ppm)
    {
        const int nrow = spectrum.nrow();
        const int ncol = spectrum.ncol();

        if (ppm.size() != 4) {
            return false;
        }

        ndata_frq = ncol;
        ndata_frq_indirect = nrow;
        nspectra = 1;

        begin1 = ppm[0];
        step1 = ppm[1];
        begin2 = ppm[2];
        step2 = ppm[3];
        stop1 = begin1 + step1 * (ndata_frq - 1);
        stop2 = begin2 + step2 * (ndata_frq_indirect - 1);

        spectrum_real_real.assign(static_cast<size_t>(ndata_frq) * ndata_frq_indirect, 0.0f);
        spectrum_real_imag.assign(spectrum_real_real.size(), 0.0f);
        spectrum_imag_real.assign(spectrum_real_real.size(), 0.0f);
        spectrum_imag_imag.assign(spectrum_real_real.size(), 0.0f);

        for (int j = 0; j < nrow; ++j) {
            for (int i = 0; i < ncol; ++i) {
                spectrum_real_real[static_cast<size_t>(j) * ncol + i] =
                    static_cast<float>(spectrum(j, i));
            }
        }

        spect = spectrum_real_real.data();
        return true;
    }

    void set_constant_noise(double noise)
    {
        noise_level = static_cast<float>(noise);
        noise_level_columns.assign(ndata_frq, noise_level);
        noise_level_rows.assign(ndata_frq_indirect, noise_level);
    }

    void estimate_noise_from_mad()
    {
        noise_level_columns.clear();
        noise_level_rows.clear();
        estimate_noise_level_mad();
    }

    void estimate_noise_from_default()
    {
        noise_level_columns.clear();
        noise_level_rows.clear();
        estimate_noise_level();
    }

    DataFrame peak_dataframe()
    {
        get_ppm_from_point();

        NumericVector conf(p1.size());
        for (size_t i = 0; i < p1.size(); ++i) {
            conf[i] = std::min(p_confidencex[i], p_confidencey[i]);
        }

        return DataFrame::create(
            _["x"] = wrap(p1),
            _["y"] = wrap(p2),
            _["ppm_x"] = wrap(p1_ppm),
            _["ppm_y"] = wrap(p2_ppm),
            _["intensity"] = wrap(p_intensity),
            _["sigmax"] = wrap(sigmax),
            _["sigmay"] = wrap(sigmay),
            _["gammax"] = wrap(gammax),
            _["gammay"] = wrap(gammay),
            _["confidence"] = conf);
    }

    List result_list()
    {
        return List::create(
            _["peaks"] = peak_dataframe(),
            _["median_width"] = NumericVector::create(median_width_x, median_width_y),
            _["noise_level"] = static_cast<double>(noise_level));
    }
};

static void configure_picker(spectrum_pick &picker,
                             double scale,
                             double scale2,
                             int model)
{
    picker.set_scale(scale, scale2);
    picker.set_scale_negative(scale, scale2);
    picker.set_model_selection(model);
}

static void maybe_adjust_ppp(spectrum_pick &picker,
                             int model,
                             bool auto_ppp)
{
    if (auto_ppp) {
        const double target_width = model == 1 ? 12.0 : 6.0;
        picker.adjust_ppp_of_spectrum(target_width);
    }
}

class DeepPicker1dAdapter : public spectrum_pick_1d
{
public:
    bool load_from_vector(const NumericVector &spectrum,
                          const NumericVector &ppm)
    {
        if (ppm.size() != 3) {
            return false;
        }

        std::vector<float> data(static_cast<size_t>(spectrum.size()));
        std::transform(spectrum.begin(), spectrum.end(), data.begin(),
                       [](double x) { return static_cast<float>(x); });

        return set_spectrum_from_data(data, ppm[0], ppm[1], ppm[2]);
    }

    void estimate_noise_from_default()
    {
        est_noise_level();
    }

    void zero_negative_points()
    {
        for (size_t i = 0; i < spectrum_real.size(); ++i) {
            spectrum_real[i] = std::max(spectrum_real[i], 0.0f);
        }
    }

    DataFrame peak_dataframe()
    {
        spectrum_1d_peaks peaks;
        get_peaks(peaks);

        return DataFrame::create(
            _["x"] = wrap(peaks.x),
            _["ppm"] = wrap(peaks.ppm),
            _["height"] = wrap(peaks.a),
            _["sigmax"] = wrap(peaks.sigmax),
            _["gammax"] = wrap(peaks.gammax),
            _["volume"] = wrap(peaks.volume),
            _["confidence"] = wrap(peaks.confidence));
    }
};

class DeepReader1dAdapter : public fid_1d
{
public:
    bool load_file(const std::string &path)
    {
        return read_spectrum(path);
    }

    NumericVector spectrum_vector()
    {
        const std::vector<float> real = get_spectrum_real();
        NumericVector out(real.size());
        CharacterVector names(real.size());

        for (size_t i = 0; i < real.size(); ++i) {
            out[i] = real[i];
            names[i] = format_ppm(begin1 + step1 * static_cast<double>(i));
        }

        out.attr("names") = names;
        return out;
    }

private:
    static std::string format_ppm(double x)
    {
        std::ostringstream oss;
        oss << std::setprecision(12) << x;
        return oss.str();
    }
};

class DeepReader2dAdapter : public fid_2d
{
public:
    bool load_file(const std::string &path)
    {
        return read_spectrum(path);
    }

    NumericMatrix spectrum_matrix()
    {
        int ncol = 0;
        int nrow = 0;
        double begins[2];
        double steps[2];

        get_dim(&ncol, &nrow);
        get_ppm_infor(begins, steps);

        NumericMatrix out(nrow, ncol);
        CharacterVector rownames(nrow);
        CharacterVector colnames(ncol);
        float *data = get_spect_data();

        for (int j = 0; j < nrow; ++j) {
            rownames[j] = format_ppm(begins[1] + steps[1] * static_cast<double>(j));
            for (int i = 0; i < ncol; ++i) {
                out(j, i) = data[static_cast<size_t>(j) * ncol + i];
            }
        }

        for (int i = 0; i < ncol; ++i) {
            colnames[i] = format_ppm(begins[0] + steps[0] * static_cast<double>(i));
        }

        out.attr("dimnames") = List::create(rownames, colnames);
        return out;
    }

private:
    static std::string format_ppm(double x)
    {
        std::ostringstream oss;
        oss << std::setprecision(12) << x;
        return oss.str();
    }
};

static void maybe_adjust_ppp_1d(spectrum_pick_1d &picker,
                                int model,
                                bool auto_ppp,
                                double interp_step)
{
    const bool interpolate = std::abs(interp_step - 1.0) > 0.01;

    if (interpolate) {
        picker.interpolate_spectrum(interp_step);
        return;
    }

    if (auto_ppp) {
        const double target_width = model == 1 ? 12.0 : 6.0;
        picker.adjust_ppp_of_spectrum(target_width);
        picker.adjust_ppp_of_spectrum(target_width);
    }
}

extern "C" SEXP C_deeppicker_pick_matrix(SEXP spectrumSEXP,
                                         SEXP ppmSEXP,
                                         SEXP noiseSEXP,
                                         SEXP scaleSEXP,
                                         SEXP scale2SEXP,
                                         SEXP modelSEXP,
                                         SEXP autoPppSEXP,
                                         SEXP t1NoiseSEXP,
                                         SEXP negativeSEXP,
                                         SEXP debugFlagSEXP,
                                         SEXP outFormatSEXP)
{
    NumericMatrix spectrum(spectrumSEXP);
    NumericVector ppm(ppmSEXP);
    const bool estimate_noise = Rf_isNull(noiseSEXP);
    const double noise = estimate_noise ? 0.0 : as<double>(noiseSEXP);
    const double scale = as<double>(scaleSEXP);
    const double scale2 = as<double>(scale2SEXP);
    const int model = as<int>(modelSEXP);
    const bool auto_ppp = as<bool>(autoPppSEXP);
    const bool t1_noise = as<bool>(t1NoiseSEXP);
    const bool negative = as<bool>(negativeSEXP);
    const int debug_flag = as<int>(debugFlagSEXP);
    const std::string out_format = as<std::string>(outFormatSEXP);

    DeepPickerAdapter picker;
    if (!picker.load_from_matrix(spectrum, ppm)) {
        stop("Failed to initialize DEEP Picker from the input matrix.");
    }

    if (estimate_noise) {
        picker.estimate_noise_from_default();
    } else {
        picker.set_constant_noise(noise);
    }

    configure_picker(picker, scale, scale2, model);
    maybe_adjust_ppp(picker, model, auto_ppp);

    if (!picker.ann_peak_picking(debug_flag, t1_noise ? 1 : 0, negative)) {
        stop("DEEP Picker peak picking failed.");
    }

    if (out_format == "list") {
        return picker.result_list();
    }

    return picker.peak_dataframe();
}

extern "C" SEXP C_deeppicker_pick_file(SEXP pathSEXP,
                                       SEXP outPathSEXP,
                                       SEXP scaleSEXP,
                                       SEXP scale2SEXP,
                                       SEXP modelSEXP,
                                       SEXP autoPppSEXP,
                                       SEXP t1NoiseSEXP,
                                       SEXP negativeSEXP)
{
    const std::string path = as<std::string>(pathSEXP);
    const std::string out_path = as<std::string>(outPathSEXP);
    const double scale = as<double>(scaleSEXP);
    const double scale2 = as<double>(scale2SEXP);
    const int model = as<int>(modelSEXP);
    const bool auto_ppp = as<bool>(autoPppSEXP);
    const bool t1_noise = as<bool>(t1NoiseSEXP);
    const bool negative = as<bool>(negativeSEXP);

    spectrum_pick picker;

    if (!picker.init(path)) {
        stop("Failed to read spectrum file '%s'.", path);
    }

    configure_picker(picker, scale, scale2, model);
    maybe_adjust_ppp(picker, model, auto_ppp);

    if (!picker.ann_peak_picking(0, t1_noise ? 1 : 0, negative)) {
        stop("DEEP Picker peak picking failed for '%s'.", path);
    }

    if (!picker.print_peaks_picking(out_path)) {
        stop("Failed to write peak table '%s'.", out_path);
    }

    return R_NilValue;
}

extern "C" SEXP C_deeppicker_pick_1d(SEXP spectrumSEXP,
                                     SEXP ppmSEXP,
                                     SEXP noiseSEXP,
                                     SEXP scaleSEXP,
                                     SEXP scale2SEXP,
                                     SEXP modelSEXP,
                                     SEXP autoPppSEXP,
                                     SEXP interpStepSEXP,
                                     SEXP negativeSEXP)
{
    NumericVector spectrum(spectrumSEXP);
    NumericVector ppm(ppmSEXP);
    const bool estimate_noise = Rf_isNull(noiseSEXP);
    const double noise = estimate_noise ? 0.0 : as<double>(noiseSEXP);
    const double scale = as<double>(scaleSEXP);
    const double scale2 = as<double>(scale2SEXP);
    const int model = as<int>(modelSEXP);
    const bool auto_ppp = as<bool>(autoPppSEXP);
    const double interp_step = as<double>(interpStepSEXP);
    const bool negative = as<bool>(negativeSEXP);

    DeepPicker1dAdapter picker;
    picker.init(scale, scale2, noise);

    if (!picker.load_from_vector(spectrum, ppm)) {
        stop("Failed to initialize DEEP Picker 1D from the input vector.");
    }

    if (estimate_noise) {
        picker.estimate_noise_from_default();
    }

    if (!negative) {
        picker.zero_negative_points();
    }

    picker.init_mod(model);
    maybe_adjust_ppp_1d(picker, model, auto_ppp, interp_step);

    if (!picker.spectrum_pick_1d_work(negative)) {
        stop("DEEP Picker 1D peak picking failed.");
    }

    return picker.peak_dataframe();
}

extern "C" SEXP C_deeppicker_pick_1d_file(SEXP pathSEXP,
                                          SEXP outPathSEXP,
                                          SEXP scaleSEXP,
                                          SEXP scale2SEXP,
                                          SEXP noiseSEXP,
                                          SEXP modelSEXP,
                                          SEXP autoPppSEXP,
                                          SEXP interpStepSEXP,
                                          SEXP negativeSEXP)
{
    const std::string path = as<std::string>(pathSEXP);
    const std::string out_path = as<std::string>(outPathSEXP);
    const double scale = as<double>(scaleSEXP);
    const double scale2 = as<double>(scale2SEXP);
    const double noise = as<double>(noiseSEXP);
    const int model = as<int>(modelSEXP);
    const bool auto_ppp = as<bool>(autoPppSEXP);
    const double interp_step = as<double>(interpStepSEXP);
    const bool negative = as<bool>(negativeSEXP);

    spectrum_pick_1d picker;
    picker.init(scale, scale2, noise);
    picker.init_mod(model);

    if (!picker.read_spectrum(path, negative)) {
        stop("Failed to read 1D spectrum file '%s'.", path);
    }

    maybe_adjust_ppp_1d(picker, model, auto_ppp, interp_step);

    if (!picker.spectrum_pick_1d_work(negative)) {
        stop("DEEP Picker 1D peak picking failed for '%s'.", path);
    }

    if (!picker.print_peaks(out_path)) {
        stop("Failed to write 1D peak table '%s'.", out_path);
    }

    return R_NilValue;
}

extern "C" SEXP C_deeppicker_read_1d(SEXP pathSEXP)
{
    const std::string path = as<std::string>(pathSEXP);
    DeepReader1dAdapter reader;

    if (!reader.load_file(path)) {
        stop("Failed to read 1D spectrum file '%s'.", path);
    }

    return reader.spectrum_vector();
}

extern "C" SEXP C_deeppicker_read_2d(SEXP pathSEXP)
{
    const std::string path = as<std::string>(pathSEXP);
    DeepReader2dAdapter reader;

    if (!reader.load_file(path)) {
        stop("Failed to read 2D spectrum file '%s'.", path);
    }

    return reader.spectrum_matrix();
}
