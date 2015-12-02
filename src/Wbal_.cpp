#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> Wbal_(NumericVector t, NumericVector P, double CP)
{
  double n = P.size();
  // Create a vector of supra- and sub-CP sections.
  std::vector<double> section(n);
  for (double i = 0; i < n; ++i)
    section[i] = (P[i] <= CP) ? 1 : 2;
  // Generate vectors required for W' expenditure calculation.
  std::vector<double> dt(n), dWexp(n), tu(n), DCP(n), tau(n), rec(n);
  for (double i = 1; i < n; ++i) // NB: Starts at the **second** element.
  {
    dt[i] = t[i] - t[i - 1];
    if (section[i] == 2) // Is this a supraCP section?
    {
      dWexp[i] = dt[i] * (P[i] - CP);
      if (dWexp[i - 1] != 0)
        dWexp[i] = dWexp[i - 1] + dWexp[i];
    }
    else if (section[i] == 1)
    {
      tu[i] = dt[i];
      if (tu[i - 1] != 0)
        tu[i] = tu[i - 1] + tu[i];
      DCP[i] = CP - P[i];
      tau[i] = 546 * std::exp(-0.01 * DCP[i]) + 316;
      rec[i] = std::exp(-tu[i] / tau[i]);
    }
  }
  // Generate W' expenditure from the above.
  std::vector<double> Wexp(n);
  double Wtmp = 0;
  for (double i = 0; i < n; ++i)
  {
    if (section[i] == 2)
    {
      Wexp[i] = dWexp[i] + Wtmp;
      if (dWexp[i + 1] == 0)
        Wtmp = Wexp[i];
    }
    else if (section[i] == 1)
    {
      if (Wtmp != 0)
      {
        Wexp[i] = Wtmp * rec[i];
        if (dWexp[i + 1] != 0)
          Wtmp = Wexp[i];
      }
    }
  }
  return Wexp;
}
