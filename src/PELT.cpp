#include <Rcpp.h>
#include "costFunctions.h"
#include "logging.h"
#include <forward_list>
#include <cmath>
#include <random>
#include <limits>
#include <iostream>
#include <fstream>
#include <string>
using namespace Rcpp;


// --------- // PELT // --------- //
//
// Provided some data vector, uses the PELT algorithm to return its optimal par-
// titioning
// 
// Parameters:
//  - data (vector): a vector of numeric values
//  - penalty: the value of the penalty in the penalized changepoint detection
//    model


// [[Rcpp::export]]
List PELT(NumericVector data, double penalty = 0) {
  const int n = data.size();
  
  if(penalty == 0)
    penalty = 2 * log(n);
  
  
  // Initialize incremented vectors
  
  IntegerVector changepointsForward(n + 1, 0); // changepointsForward records the optimal last change point at each OP step
  NumericVector valuesCumsum(n + 1, 0.0), costRecord(n + 1, - penalty); // valuesCumsum stores the cumsum of the data and costRecord contains the optimal model cost at each OP step
  
  
  // Initialize OP step values
  
  // ... storing it allows pruning of the first available index
  int optimalChangepoint; // stores the optimal last changepoint for the current OP step
  double optimalCost; // stores the cost for the model with optimal last changepoint for the current OP step
  
  
  // Initialize pruning step values and vectors
  
  NumericVector modelCosts (n);
  std::forward_list<int> validIndices {0}; // the available indices (decreasing)
  std::forward_list<int>::iterator i;
  std::forward_list<int>::iterator before;
  
  
  // Main loop
  
  for (int t = 1; t <= n; t++)
  {
    // update valuesCumsum
    valuesCumsum[t] =
      valuesCumsum[t - 1] + data[t - 1];
    
    // OP step
    i = validIndices.begin();
    optimalCost = std::numeric_limits<double>::infinity();
    do
    {
      modelCosts[*i] = modelCost(t, *i, valuesCumsum, costRecord);
      if (modelCosts[*i] < optimalCost)
      {
        optimalCost = modelCosts[*i] + penalty;
        optimalChangepoint = *i;
      }
      ++i;
    }
    while(i != validIndices.end());
    // END (OP step)
    
    // OP update
    costRecord[t] = optimalCost;
    changepointsForward[t] = optimalChangepoint;
    
    // if (t % 5) {
    //   validIndices.add(t);
    //   continue;
    // }
    
    // PELT step
     before = validIndices.before_begin();
     i = std::next(before);
    
    // PELT loop
    do
    {
      if (modelCosts[*i] > optimalCost) // prune as needs pruning
      {
        // remove the pruned index
        // removing the index increments the cursor i, while before stands still
        i = validIndices.erase_after(before);
      }
      else
      {
        // increment cursors
        before = i;
        ++i;
      }
    }
    while (i != validIndices.end()); // exit the loop if we may not draw a valid constraint index
    // END (PELT loop)
    
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