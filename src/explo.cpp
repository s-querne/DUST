#include <Rcpp.h>
#include "costFunctions.h"
#include "ForwardListHandler.h"
#include <forward_list>
#include <cmath>
#include <random>
#include <limits>
#include <iostream>
#include <fstream>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
List explo() {
  ForwardListHandler x;
  Rcout << "nb = " << x.length << std::endl;
  x.add(2);
  x.add(3);
  x.add(4);
  x.reset_prune();
  Rcout << "Ici " << x.read() << std::endl;
  x.next_prune();
  x.prune();
  x.prune();
  Rcout << x.check() << std::endl;
  // Rcout << "nb = " << x.length << std::endl;
  // x.reset();
  // x.next();
  // Rcout << x.read();
  List output;
  output["x"] = x.get_list();
  return output;
  // std::forward_list<int> y = x.list;
  // int i = x.next();
  // Rcout << "i = " << i << "; nb = " << x.length;
}

// [[Rcpp::export]]
void generate_runif1 (int n) {
  int u;
  for (int i = 0; i < n; i++){u = Rcpp::runif(1)[0];}
}

// [[Rcpp::export]]
void generate_runif2 (int n) {
  NumericVector randomU;
  NumericVector::iterator u;
  randomU = Rcpp::runif(n);
  u = randomU.begin();
  while(u != randomU.end()) {
    ++u;
  }
}

/*** R
microbenchmark::microbenchmark(
  generate_runif1(1e2),
  generate_runif2(1e2),
  times = 1e3
)
*/