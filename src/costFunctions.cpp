#include <Rcpp.h>
#include "costFunctions.h"
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

// [[Rcpp::export]]
double simpleTest
(
  int t,
  int i,
  int j,
  const NumericVector& valuesCumsum,
  const NumericVector& costRecord
)
{
  
  // Compute the optimal point on which to evaluate the duality function
  //
    // Denoting y_it = y[(i+1):t]; y_ji = y[(j+1):i]
  // Denoting m_it = mean(y_it); m_ji = mean(y_ji)
  // Duality function: D(mu) = Qi + mu * (Qi-Qj) - ((t - i) * m_it - mu * (i - j) * sm_ji)^2 / (t - i - mu * (i - j))
  // Denoting mu_max = (t - i) / (i - j) 
  //
    // Formula: mu* = max(0, mu_max * (1 - abs(m_it - m_ji) / sqrt((Qi - Qj) / (i-j) + m_ji^2)))
  
  double costTerm = costRecord[i] - costRecord[j]; // Qi - Qj
  
  int objectiveLength = t - i;
  double objectiveSum = valuesCumsum[t] - valuesCumsum[i];
  double objectiveMean = objectiveSum / objectiveLength; // m_it
  double squareMean = pow(objectiveMean, 2);
  
  int gapLength = i - j;
  double gapSum = valuesCumsum[i] - valuesCumsum[j];
  double gapMean = gapSum / gapLength; // m_ji
  
  // Case 1: mu* = 0
  // deduce the following condition from the formula for mu*
    if (costTerm / gapLength <= squareMean - 2 * objectiveMean * gapMean)
      return costRecord[i] - objectiveLength * squareMean; // D(0)
  
  // Case 2: mu* > 0
  // compute mu* from the formula
  double optimalMu = (static_cast<double>(objectiveLength) / gapLength) * (1 - fabs(objectiveMean - gapMean)
                                                                           / sqrt(costTerm / gapLength + pow(gapMean, 2)));
  // compute and return D(mu*)
  return - pow(objectiveSum - optimalMu * gapSum, 2)
  / (objectiveLength - optimalMu * gapLength)
  + costRecord[i] + optimalMu * costTerm;
}


// --------- // modelCost // --------- //
//
// Computes the gaussian model cost in the OP algorithm at step t with cursor at i
// 
// Parameters:
//  - t: the current step in the OP main loop
//  - i: the current step in the OP sub loop
//  - valuesCumsum (vector): the cumulative sum of the data (index 0 == 0)
//  - costRecord (vector): the optimal model cost at each OP index

double modelCost
(
  int t,
  int i,
  NumericVector& valuesCumsum,
  NumericVector& costRecord
)
{
  return costRecord[i] - pow(valuesCumsum[t] - valuesCumsum[i], 2) / (t - i);
}
