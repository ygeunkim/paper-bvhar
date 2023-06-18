#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Eigen::MatrixXd ldlt_lower(Eigen::MatrixXd x) {
  return x.ldlt().matrixL();
}

// [[Rcpp::export]]
Eigen::MatrixXd ldlt_upper(Eigen::MatrixXd x) {
  return x.ldlt().matrixU();
}
