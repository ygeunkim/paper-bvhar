#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Eigen::MatrixXd ldlt_inv_lower(Eigen::MatrixXd x) {
  return x.inverse().ldlt().matrixL();
}

// [[Rcpp::export]]
Eigen::MatrixXd llt_inv_lower(Eigen::MatrixXd x) {
  return x.inverse().llt().matrixL();
}

// [[Rcpp::export]]
Eigen::MatrixXd ldlt_inv_upper(Eigen::MatrixXd x) {
  return x.inverse().ldlt().matrixU();
}

// [[Rcpp::export]]
Eigen::MatrixXd llt_inv_upper(Eigen::MatrixXd x) {
  return x.inverse().llt().matrixU();
}
