#include<Rcpp.h>
#include<forward_list>
#include<cmath>
#include<random>
#include<limits>
#include<iostream>
#include<fstream>
#include<string>
using namespace Rcpp;

// [[Rcpp::export]]
void logging(std::string txt)  {
  std::ofstream file("log.txt", std::ios_base::app);
  file << txt << "\n";
  file.close();
}

// [[Rcpp::export]]
void resetLogging(String path)  {
  std::ofstream file(path);
  file << "";
  file.close();
}