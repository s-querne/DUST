#ifndef COSTFUNCTIONS_H
#define COSTFUNCTIONS_H

#include <Rcpp.h>
#include <forward_list>
#include <cmath>
#include <random>
#include <limits>
#include <iostream>
#include <fstream>
#include <string>
using namespace Rcpp;

// --------- // Simple test // --------- //
//
// Performs the simple lagrangian duality test to find the lowest "visible " point
// of a given cost function
// 
// Parameters:
//  - t: the current step in the OP main loop
//  - i: the current step in the pruning sub loop
//  - j: a random index drawn from the available indices, less than i
//  - valuesCumsum (vector): the cumulative sum of the data (index 0 == 0)
//  - costRecord (vector): the optimal model cost at each OP index

double simpleTest
(
    int t,
    int i,
    int j,
    const NumericVector& valuesCumsum,
    const NumericVector& costRecord
);


// --------- // modelCost // --------- //
//
// Computes the gaussian model cost in the OP algorithm at step t with cursor at i
// 
// Parameters:
//  - t: the current step in the OP main loop
//  - i: the current step in the OP sub loop
//  - penalty: the penalty value in the penalized changepoint detection model
//  - valuesCumsum (vector): the cumulative sum of the data (index 0 == 0)
//  - costRecord (vector): the optimal model cost at each OP index

double modelCost
(
    int t,
    int i,
    NumericVector& valuesCumsum,
    NumericVector& costRecord
);

#endif