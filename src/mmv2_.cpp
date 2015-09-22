#include <Rcpp.h>
using namespace Rcpp;
//' Efficient maximal mean values.
//'
//' A more efficient implementation of \code{\link{mmv}}. Simply takes
//' a vector (\code{x}) of values and rolls over them element wise by windows
//' defined in \code{pds}. Returns a vector of maximum mean values for each
//' window size.
//'
//' @param x a numeric vector of values.
//' @param pds window size(s) (in element units) for which to
//'   generate maximum mean values.
//'
//' @return a vector of \code{length(pds)}.
//'
//' @export
// [[Rcpp::export]]
std::vector<double> mmv2(NumericVector x, NumericVector pds)
{
  double mn, n = x.size(), pdn = pds.size();
  std::vector<double> out_max(pdn);
  for (unsigned int p = 0; p < pdn; ++p)      // Rolling over periods.
  {
    double st = (pds[p] - 1);
    for (unsigned int i = st; i < n; ++i)     // Roll to the end of x, row-wise.
    {
      double sum(0); // Reset.
      for (double c = (i - st); c <= i; ++c)  // Calculate sum.
        sum += x[c];
      mn = sum / pds[p]; // Mean.
      if (mn > out_max[p])
        out_max[p] = mn;
    }
  }
  return out_max;
}
