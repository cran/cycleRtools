#include <Rcpp.h>
using namespace Rcpp;
//' Efficient maximal mean values.
//'
//' A more efficient implementation of \code{\link{mmv}}. Simply takes a vector
//' (\code{x}) of values and rolls over them element wise by defined windows.
//' Returns a vector of maximum mean values for each window size.
//'
//' @param x a numeric vector of values.
//' @param windows window size(s) (in element units) for which to
//'   generate maximum mean values.
//'
//' @return a vector of \code{length(windows)}.
//'
//' @export
// [[Rcpp::export]]
std::vector<double> mmv2(NumericVector x, NumericVector windows)
{
  double mn, n = x.size(), winsz = windows.size();
  std::vector<double> out_max(winsz);
  for (unsigned int p = 0; p < winsz; ++p)      // Rolling over periods.
  {
    double st = (windows[p] - 1);
    for (unsigned int i = st; i < n; ++i)     // Roll to the end of x, row-wise.
    {
      double sum(0); // Reset.
      for (double c = (i - st); c <= i; ++c)  // Calculate sum.
        sum += x[c];
      mn = sum / windows[p]; // Mean.
      if (mn > out_max[p])
        out_max[p] = mn;
    }
  }
  return out_max;
}
