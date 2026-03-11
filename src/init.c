#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP C_deeppicker_pick_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                                     SEXP, SEXP, SEXP, SEXP);
extern SEXP C_deeppicker_pick_file(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                                   SEXP, SEXP);
extern SEXP C_deeppicker_pick_1d(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                                 SEXP, SEXP, SEXP);
extern SEXP C_deeppicker_pick_1d_file(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                                      SEXP, SEXP, SEXP);
extern SEXP C_deeppicker_read_1d(SEXP, SEXP);
extern SEXP C_deeppicker_read_2d(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_deeppicker_pick_matrix", (DL_FUNC) &C_deeppicker_pick_matrix, 12},
    {"C_deeppicker_pick_file", (DL_FUNC) &C_deeppicker_pick_file, 9},
    {"C_deeppicker_pick_1d", (DL_FUNC) &C_deeppicker_pick_1d, 10},
    {"C_deeppicker_pick_1d_file", (DL_FUNC) &C_deeppicker_pick_1d_file, 10},
    {"C_deeppicker_read_1d", (DL_FUNC) &C_deeppicker_read_1d, 2},
    {"C_deeppicker_read_2d", (DL_FUNC) &C_deeppicker_read_2d, 2},
    {NULL, NULL, 0}
};

void R_init_deeppicker(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
