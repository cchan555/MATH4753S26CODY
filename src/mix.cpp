#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector dmix_cpp(NumericVector x,
                       double d1, double s1,
                       double d2, double s2,
                       double w,
                       bool log = false) {

  int n = x.size();
  NumericVector out(n);

  for (int i = 0; i < n; i++) {
    double val =
      w * R::dnorm(x[i], d1, s1, false) +
      (1 - w) * R::dnorm(x[i], d2, s2, false);

    out[i] = log ? std::log(val) : val;
  }

  return out;
}

// [[Rcpp::export]]
NumericVector pmix_cpp(NumericVector q,
                       double d1, double s1,
                       double d2, double s2,
                       double w) {

  int n = q.size();
  NumericVector out(n);

  for (int i = 0; i < n; i++) {
    out[i] =
      w * R::pnorm(q[i], d1, s1, true, false) +
      (1 - w) * R::pnorm(q[i], d2, s2, true, false);
  }

  return out;
}

// [[Rcpp::export]]
NumericVector rmix_cpp(int n,
                       double d1, double s1,
                       double d2, double s2,
                       double w) {

  NumericVector out(n);

  for (int i = 0; i < n; i++) {
    double u = R::runif(0.0, 1.0);

    if (u < w) {
      out[i] = R::rnorm(d1, s1);
    } else {
      out[i] = R::rnorm(d2, s2);
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector qmix_cpp(NumericVector p,
                       double d1, double s1,
                       double d2, double s2,
                       double w) {

  int n = p.size();
  NumericVector out(n);

  for (int i = 0; i < n; i++) {
    double lo = -10, hi = 10;

    for (int iter = 0; iter < 100; iter++) {
      double mid = (lo + hi) / 2.0;

      double val =
        w * R::pnorm(mid, d1, s1, true, false) +
        (1 - w) * R::pnorm(mid, d2, s2, true, false);

      if (val < p[i]) lo = mid;
      else hi = mid;
    }

    out[i] = (lo + hi) / 2.0;
  }

  return out;
}
