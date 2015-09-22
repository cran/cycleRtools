#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> rollmean_(NumericVector x, unsigned int pd)
{
  // Rolling by row, so need to account for indexes starting at 0.
  // NB: st ("start") is the first row on which to start rolling.
  double st = (pd - 1), n = x.size();
  std::vector<double> out(n);
  for (unsigned int i = st; i < n; ++i)
  {
    double sum(0); // Reset.
    for (double c = (i - st); c <= i; ++c)
      sum += x[c];
    out[i] = sum / pd;
  }
  return out;
}
