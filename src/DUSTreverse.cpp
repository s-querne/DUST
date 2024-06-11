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

// [[Rcpp::export]]
double simpleTestReverse
(
    int t,
    int i,
    int j,
    const NumericVector& valuesCumsum,
    const NumericVector& costRecord
)
{
  double costTerm = costRecord[i] - costRecord[j];
  
  int objectiveLength = t - i;
  double objectiveSum = valuesCumsum[t] - valuesCumsum[i];
  double objectiveMean = objectiveSum / objectiveLength;
  double squareMean = pow(objectiveMean, 2);
  
  int gapLength = i - j;
  double gapSum = valuesCumsum[i] - valuesCumsum[j];
  double gapMean = gapSum / gapLength;
  
  // Cas 1: Mu = 0
  if (costTerm / gapLength <= squareMean - 2 * objectiveMean * gapMean)
    return costRecord[i] - objectiveLength * squareMean;
  
  // Cas 2: Mu > 0
  double optimalMu = (static_cast<double>(objectiveLength) / gapLength) * (1 - fabs(objectiveMean - gapMean)
                                                                      / sqrt(costTerm / gapLength + pow(gapMean, 2)));
  
  // Rcout << "mu " << optimalMu << std::endl;
  return - pow(objectiveSum - optimalMu * gapSum, 2)
    / (objectiveLength - optimalMu * gapLength)
    + costRecord[i] + optimalMu * costTerm;
}


// [[Rcpp::export]]
List DUSTreverse(NumericVector data, double penalty = 0) {
  const int n = data.size();
  
  if(penalty == 0)
    penalty = 2 * log(n);
  
  IntegerVector changepointsForward(n + 1, 0), indicesCount(n);
  NumericVector valuesCumsum(n + 1, 0.0), costRecord(n + 1, - penalty);
  double lastCost;
  
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
  std::vector<int*> indicesPointers {&(*std::next(i)), &(*i)};
  std::vector<int*>::reverse_iterator pointerIt;
  std::vector<int*>::iterator pointerForward;
  
  NumericVector randomU;
  NumericVector::iterator u;
  
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
      lastCost = costRecord[*i] + penalty - pow(valuesCumsum[t] - valuesCumsum[*i], 2) / (t - (*i));
      if (lastCost < optimalCost)
      {
        optimalCost = lastCost;
        optimalChangepoint = *i;
      }
      ++i;
    }

    costRecord[t] = optimalCost;
    changepointsForward[t] = optimalChangepoint;


    // Elagage
    nbConstraints = nb - 1;
    randomU = Rcpp::runif(nbConstraints);
    u = randomU.begin();

    i = validIndices.begin();
    before = validIndices.before_begin();
    pointerIt = indicesPointers.rbegin();
    j = indicesPointers[floor(nbConstraints * (*u))];
    ++u;

    // DUSTflist
    do
    {
      testValue = simpleTestReverse(t, *i, *j, valuesCumsum, costRecord);
      if (testValue > optimalCost)
      {
        pointerForward = indicesPointers.erase(std::next(pointerIt).base());
        i = validIndices.erase_after(before);
        nb--;
        
        nbConstraints--;
        pointerIt = std::make_reverse_iterator(pointerForward);
        j = indicesPointers[floor(nbConstraints * (*u))];
        ++u;
      }
      else
      {
        nbConstraints--;
        j = indicesPointers[floor(nbConstraints * (*u))];
        ++u;
        
        before = i;
        ++i;
        ++pointerIt;
      }
    }
    while (nbConstraints > 0);

    if (lastCost > optimalCost + penalty) {
       indicesPointers.erase(indicesPointers.begin());
       validIndices.erase_after(before);
       nb--;
    }

    validIndices.push_front(t);
    indicesPointers.push_back(&(*validIndices.begin()));
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
