#ifndef LOGGING
#define LOGGING

#include<Rcpp.h>
#include<forward_list>
#include<cmath>
#include<random>
#include<limits>
#include<iostream>
#include<fstream>
#include<string>
using namespace Rcpp;

void logging(std::string txt);

void resetLogging(String path);

#endif