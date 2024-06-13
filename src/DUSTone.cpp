#include <Rcpp.h>
#include "costFunctions.h"
#include "ForwardListHandler.h"
#include "logging.h"
#include <forward_list>
#include <cmath>
#include <random>
#include <limits>
#include <iostream>
#include <fstream>
#include <string>
using namespace Rcpp;


// --------- // DUST // --------- //
//
// Provided some data vector, uses the DUST algorithm to return its optimal par-
// titioning
// 
// Parameters:
//  - data (vector): a vector of numeric values
//  - penalty: the value of the penalty in the penalized changepoint detection
//    model


// [[Rcpp::export]]
List DUSTone(NumericVector data, double penalty = 0, double alpha = 1e-9) {
  const int n = data.size();
  
  if(penalty == 0)
    penalty = 2 * log(n);
  
  
  // Initialize incremented vectors
  
  IntegerVector changepointsForward(n + 1, 0); // changepointsForward records the optimal last change point at each OP step
  NumericVector valuesCumsum(n + 1, 0.0), costRecord(n + 1, - penalty); // valuesCumsum stores the cumsum of the data and costRecord contains the optimal model cost at each OP step
  
  
  // Initialize OP step values
  
  double lastCost; // temporarily stores the cost for the model with last changepoint at some i, then keeps the cost of the model with last changepoint at the first possible index in the t-th OP step ...
  // ... storing it allows pruning of the first available index
  int optimalChangepoint; // stores the optimal last changepoint for the current OP step
  double optimalCost; // stores the cost for the model with optimal last changepoint for the current OP step
  
  // Initialize pruning step values and vectors
  
  double testValue; // the value to be checked vs. the test threshold = optimalCost
  
  std::forward_list<int> validIndices {1, 0};
  std::forward_list<int>::iterator before, i, j;
  
  
  // First OP step (t = 1)
  
  valuesCumsum[1] = data[0];
  costRecord[1] = - pow(data[0], 2);
  changepointsForward[1] = 0;
  
  
  // Main loop
  
  for (int t = 2; t <= n; t++)
  {
    // update valuesCumsum
    valuesCumsum[t] =
      valuesCumsum[t - 1] + data[t - 1];
    
    // OP step
    i = validIndices.begin();
    optimalCost = std::numeric_limits<double>::infinity();
    do
    {
      lastCost = modelCost(t, *i, valuesCumsum, costRecord);
      if (lastCost < optimalCost)
      {
        optimalCost = lastCost;
        optimalChangepoint = *i;
      }
      ++i;
    }
    while(i != validIndices.end());
    // END (OP step)
    
    // OP update
    optimalCost += penalty;
    costRecord[t] = optimalCost;
    changepointsForward[t] = optimalChangepoint;
    
    // if (t % 5) {
    //   validIndices.add(t);
    //   continue;
    // }
    
    // DUST step
    before = validIndices.before_begin();
    i = std::next(before);
    j = std::next(i);

    // DUST loop
    do
    {
      testValue = simpleTest(t, *i, *j, valuesCumsum, costRecord); // compute test value
      if (testValue > optimalCost) // prune as needs pruning
      {
        // remove the pruned index and its pointer
        // removing the elements increments the cursors i and pointerIt, while before stands still
        i = validIndices.erase_after(before);
      }
      else
      {
        // increment all cursors
        before = i;
        i = j;
      }
      ++j;
    }
    while (j != validIndices.end()); // exit the loop if we may not draw a valid constraint index
    // END (DUST loop)

    // Prune the last index (analoguous with a null (mu* = 0) duality simple test)
    if (lastCost > optimalCost) {
      validIndices.erase_after(before);
    }
    
    // update the available indices
    validIndices.push_front(t);
  }
  
  // Backtrack des changepoints
  std::forward_list<int> changepoints {n};
  for (int newChangepoint = changepointsForward[n]; newChangepoint != 0; newChangepoint = changepointsForward[newChangepoint])
  {
    changepoints.push_front(newChangepoint);
  }
  
  // Output
  List output;
  output["changepoints"] = changepoints;
  output["lastIndexSet"] = validIndices;
  output["costQ"] = costRecord;
  
  return output;
}