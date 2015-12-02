// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// diff_section
std::vector<double> diff_section(NumericVector x);
RcppExport SEXP cycleRtools_diff_section(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(diff_section(x));
    return __result;
END_RCPP
}
// mmv2
std::vector<double> mmv2(NumericVector x, NumericVector windows);
RcppExport SEXP cycleRtools_mmv2(SEXP xSEXP, SEXP windowsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type windows(windowsSEXP);
    __result = Rcpp::wrap(mmv2(x, windows));
    return __result;
END_RCPP
}
// rollmean_
std::vector<double> rollmean_(NumericVector x, unsigned int window);
RcppExport SEXP cycleRtools_rollmean_(SEXP xSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type window(windowSEXP);
    __result = Rcpp::wrap(rollmean_(x, window));
    return __result;
END_RCPP
}
// rollmean_ema_
std::vector<double> rollmean_ema_(NumericVector x, unsigned int window, NumericVector wt);
RcppExport SEXP cycleRtools_rollmean_ema_(SEXP xSEXP, SEXP windowSEXP, SEXP wtSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type window(windowSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wt(wtSEXP);
    __result = Rcpp::wrap(rollmean_ema_(x, window, wt));
    return __result;
END_RCPP
}
// Wbal_
std::vector<double> Wbal_(NumericVector t, NumericVector P, double CP);
RcppExport SEXP cycleRtools_Wbal_(SEXP tSEXP, SEXP PSEXP, SEXP CPSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type P(PSEXP);
    Rcpp::traits::input_parameter< double >::type CP(CPSEXP);
    __result = Rcpp::wrap(Wbal_(t, P, CP));
    return __result;
END_RCPP
}
// zone_index_
std::vector<double> zone_index_(NumericVector x, NumericVector zb);
RcppExport SEXP cycleRtools_zone_index_(SEXP xSEXP, SEXP zbSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type zb(zbSEXP);
    __result = Rcpp::wrap(zone_index_(x, zb));
    return __result;
END_RCPP
}
