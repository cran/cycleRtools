#include <Rcpp.h>
using namespace Rcpp;
//' Section data according to breaks.
//'
//' Generates a vector of "section" values/levels according to differences in
//' the supplied vector. The function simply rolls over \code{x}, incrementing
//' the return vector every time there is a significant break in the pattern of
//' differences between adjacent elements of \code{x}. In practical terms, if
//' \code{x} is a series of timestamp values (see example), every time there is
//' a significant break in the timer (> 10 sec currently), the return vector is
//' incremented by 1.
//'
//' @param x a numeric vector (e.g. a timer column) that increments uniformly.
//'   When there is a \strong{significant} break in this uniformity, a new
//'   section is created, and so forth.
//'
//' @return a vector of the same shape as x.
//'
//' @examples
//' t_sec <- c(1:10, 40:60, 100:150)       # Discontinuous timer values.
//' pwr   <- runif(length(t_sec), 0, 400)  # Some power values.
//' x     <- data.frame(t_sec, pwr)
//' # Generate section levels.
//' x$section <- diff_section(x$t_sec)
//' print(x)
//'
//' @export
// [[Rcpp::export]]
std::vector<double> diff_section(NumericVector x) {
  double n = x.size();
  std::vector<double> delta(n);
  for (double i = 1; i < n; ++i)
    delta[i] = x[i] - x[i - 1];
  std::vector<double> section(n, 1);                   // Fill with 1s.
  for (double i = 2; i < n; ++i)
  {
    section[i] = section[i - 1];
    if (section[i - 1] != section[i - 2])              // Was there just a break?
      continue;
    if ((delta[i - 1] != delta[i]) && (delta[i] > 10)) // Was this a sig. break?
      section[i] = section[i - 1] + 1;
  }
  return section;
}
