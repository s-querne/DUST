#include<Rcpp.h>
#include<forward_list>
#include<cmath>
#include<random>
#include<limits>
using namespace Rcpp;


// --------- // Simple test // --------- //
//
// Performs the simple lagrangian duality test to find the lowest "visible " point
// of a given cost function

// [[Rcpp::export]]
double simpleTestFlistP
(
    int t,
    int i,
    int pointerIt,
    const NumericVector& valuesCumsum,
    const NumericVector& costRecord
)
{
  double costTerm = costRecord[i] - costRecord[pointerIt];
  
  int objectiveLength = t - i;
  double objectiveSum = valuesCumsum[t] - valuesCumsum[i];
  double objectiveMean = objectiveSum / objectiveLength;
  
  int gapLength = i - pointerIt;
  double gapSum = valuesCumsum[i] - valuesCumsum[pointerIt];
  double gapMean = gapSum / gapLength;
  
  // Cas 1: Mu = 0
  if (costTerm / gapLength <= pow(objectiveMean, 2) - 2 * objectiveMean * gapMean)
    return costRecord[i] - objectiveLength * pow(objectiveMean, 2);
  
  // Cas 2: Mu > 0
  double optimalMu = ((1. * objectiveLength) / (1. * gapLength)) * (1 - fabs(objectiveMean - gapMean)
                                                                      / sqrt(costTerm / gapLength + pow(gapMean, 2)));
  
  // Rcout << "mu " << optimalMu << std::endl;
  return - pow(objectiveSum - optimalMu * gapSum, 2)
    / (objectiveLength - optimalMu * gapLength)
    + costRecord[i] + optimalMu * costTerm;
}


// [[Rcpp::export]]
List DUSTflistp(NumericVector data, double penalty = 0) {
  const int n = data.size();
  
  if(penalty == 0)
    penalty = 2 * log(n);
  
  IntegerVector changepointsForward(n + 1, 0), indicesCount(n);
  NumericVector valuesCumsum(n + 1, 0.0), costRecord(n + 1, - penalty);
  
  double locCost;
  
  double optimalCost;
  int optimalChangepoint;
  
  int* j;
  double testValue;
  
  // Première itération en dur
  valuesCumsum[1] = data[0];
  costRecord[1] = - pow(data[0], 2);
  changepointsForward[1] = 0;
  indicesCount[0] = 1;
  
  int nb = 2;
  
  std::forward_list<int> validIndices {1, 0};
  std::forward_list<int>::iterator i = validIndices.begin();
  std::forward_list<int>::iterator before;
  
  int nbConstraints;
  std::deque<int*> indicesPointers {&(*i), &(*std::next(i))};
  std::deque<int*>::iterator pointerIt;
  
  // Boucle principale
  for (int t = 2; t <= n; t++)
  {
    
    valuesCumsum[t] =
      valuesCumsum[t - 1] + data[t - 1];
    
    // Boucle OP
    i = validIndices.begin();
    optimalCost = std::numeric_limits<double>::infinity();
    while (i != validIndices.end())
    {
      locCost = costRecord[*i] + penalty - pow(valuesCumsum[t] - valuesCumsum[*i], 2) / (t - (*i));
      if (locCost < optimalCost)
      {
        optimalCost = locCost;
        optimalChangepoint = *i;
      }
      ++i;
    }
    
    costRecord[t] = optimalCost;
    changepointsForward[t] = optimalChangepoint;
    
    
    // Elagage
    nbConstraints = nb - 1;
    
    i = validIndices.begin();
    before = validIndices.before_begin();
    pointerIt = indicesPointers.begin();
    j = indicesPointers[nb - ceil(nbConstraints * Rcpp::runif(1)[0])];
    
    // DUSTflist
    do
    {
      testValue = simpleTestFlistP(t, *i, *j, valuesCumsum, costRecord);
      if (testValue > optimalCost)
      {
        i = validIndices.erase_after(before);
        pointerIt = indicesPointers.erase(pointerIt);
        nb--;
        j = indicesPointers[nb - ceil(nbConstraints * Rcpp::runif(1)[0])];
        nbConstraints--;
      }
      else 
      {
        j = indicesPointers[nb - ceil(nbConstraints * Rcpp::runif(1)[0])];
        before = i;
        ++i;
        ++pointerIt;
        nbConstraints--;
      }
    }
    while (nbConstraints > 0);
    
    if (locCost > optimalCost + penalty) {
      i = validIndices.erase_after(before);
      nb--;
    }
    
    validIndices.push_front(t);
    indicesPointers.push_front(&(*validIndices.begin()));
    nb++;
    indicesCount[t - 1] = nb;
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
  output["nb"] = indicesCount;
  output["lastIndexSet"] = validIndices;
  output["costQ"] = costRecord;
  
  return output;
}
