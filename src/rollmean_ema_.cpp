#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> rollmean_ema_(NumericVector x, unsigned int window,
                                 NumericVector wt)
{
  // Rolling by row, so need to account for different indexing.
  unsigned int st = (window - 1), n = x.size();
  // NB: count is the row counter, st ("start") is the first row.
  std::vector<double> out(n);
  for (unsigned int i = st; i < n; ++i)
  {
    unsigned int wcount(0); // Reset.
    for (double c = (i - st); c <= i; ++c)
      out[i] += (x[c] * wt[wcount++]);
  }
  return out;
}
