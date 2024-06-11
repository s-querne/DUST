#include<Rcpp.h>
#include<forward_list>
#include<cmath>
#include<random>
#include<limits>
#include<iostream>
#include<fstream>
#include<string>
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
List DUST(NumericVector data, double penalty = 0) {
  const int n = data.size();
  
  if(penalty == 0)
    penalty = 2 * log(n);
  
  
  // Initialize incremented vectors
  
  IntegerVector changepointsForward(n + 1, 0); // changepointsForward records the optimal last change point at each OP step
  // IntegerVector indicesCount(n); // indicesCount records the available indices count
  NumericVector valuesCumsum(n + 1, 0.0), costRecord(n + 1, - penalty); // valuesCumsum stores the cumsum of the data and costRecord contains the optimal model cost at each OP step
  
  
  // Initialize OP step values
  
  double lastCost; // temporarily stores the cost for the model with last changepoint at some i, then keeps the cost of the model with last changepoint at the first possible index in the t-th OP step ...
  // ... storing it allows pruning of the first available index
  int optimalChangepoint; // stores the optimal last changepoint for the current OP step
  double optimalCost; // stores the cost for the model with optimal last changepoint for the current OP step
  
  
  // Initialize pruning step values and vectors
  
  int* j; // some index drawn at random from the available indices, defines the constraint function in the duality simple test
  double testValue; // the value to be checked vs. the test threshold = optimalCost
  
  int nb = 2; // stores the size of validIndices
  std::forward_list<int> validIndices {1, 0}; // the available indices (decreasing)
  std::forward_list<int>::iterator i = validIndices.begin(); // cursor for the OP and DUST steps
  std::forward_list<int>::iterator before; // lagged cursor for DUST step index pruning
  
  int nbConstraints; // the amount of constraint indices to draw from
  std::vector<int*> indicesPointers {&(*std::next(i)), &(*i)}; // stores pointers to the values of validIndices
  std::vector<int*>::reverse_iterator pointerIt; // reverse cursor for the DUST step
  
  NumericVector randomU; // stores random uniform values for drawing j at random
  NumericVector::iterator u; // iterates over randomU
  
  
  // First OP step (t = 1)
  
  valuesCumsum[1] = data[0];
  costRecord[1] = - pow(data[0], 2);
  changepointsForward[1] = 0;
  // indicesCount[0] = 1;
  
  
  // Main loop
  
  for (int t = 2; t <= n; t++)
  {
    // update valuesCumsum
    valuesCumsum[t] =
      valuesCumsum[t - 1] + data[t - 1];
    
    // OP step
    i = validIndices.begin();
    optimalCost = std::numeric_limits<double>::infinity();
    while (i != validIndices.end())
    {
      lastCost = costRecord[*i] + penalty - pow(valuesCumsum[t] - valuesCumsum[*i], 2) / (t - (*i));
      if (lastCost < optimalCost)
      {
        optimalCost = lastCost;
        optimalChangepoint = *i;
      }
      ++i;
    }
    // END (OP step)
    
    // OP update
    costRecord[t] = optimalCost;
    changepointsForward[t] = optimalChangepoint;
    
    
    // DUST step
    nbConstraints = nb - 1; // j != i
    randomU = Rcpp::runif(nbConstraints); // generate uniform values
    u = randomU.begin();
    
    i = validIndices.begin();
    before = validIndices.before_begin();
    pointerIt = indicesPointers.rbegin();
    j = indicesPointers[floor(nbConstraints * (*u))]; // draw first j
    ++u; // next u
    
    // DUST loop
    do
    {
      testValue = simpleTest(t, *i, *j, valuesCumsum, costRecord); // compute test value
      if (testValue > optimalCost) // prune as needs pruning
      {
        // remove the pruned index and its pointer
        // removing the elements increments the cursors i and pointerIt, while before stands still
        pointerIt = std::vector<int*>::reverse_iterator(indicesPointers.erase(std::next(pointerIt).base()));
        i = validIndices.erase_after(before);
        nb--;
      }
      else
      {
        // increment all cursors
        before = i;
        ++i;
        ++pointerIt;
      }
      // draw next j
      nbConstraints--;
      j = indicesPointers[floor(nbConstraints * (*u))];
      ++u;
    }
    while (nbConstraints > 0 && nb > 1); // exit the loop if we may not draw a valid constraint index
    // END (DUST loop)
    
    // Prune the last index (analoguous with a null (mu* = 0) duality simple test)
    if (lastCost > optimalCost + penalty) {
      indicesPointers.erase(indicesPointers.begin());
      validIndices.erase_after(before);
      nb--;
    }
    
    // update the available indices
    validIndices.push_front(t);
    indicesPointers.push_back(&(*validIndices.begin()));
    nb++;
    // indicesCount[t - 1] = nb;
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
  // output["nb"] = indicesCount;
  output["lastIndexSet"] = validIndices;
  output["costQ"] = costRecord;
  
  return output;
}