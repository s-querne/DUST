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
double simpleTestVector
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

  int gapLength = i - j;
  double gapSum = valuesCumsum[i] - valuesCumsum[j];
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
List DUSTvector(NumericVector data, double penalty = 0) {
  const int n = data.size();

  if(penalty == 0)
    penalty = 2 * log(n);

  IntegerVector changepointsForward(n + 1, 0), indicesCount(n);
  NumericVector valuesCumsum(n + 1, 0.0), costRecord(n + 1, - penalty);

  double locCost, firstCost;

  double optimalCost;
  int optimalChangepoint;

  int j;
  double testValue;

  // Première itération en dur
  valuesCumsum[1] = data[0];
  costRecord[1] = - pow(data[0], 2);
  changepointsForward[1] = 0;
  indicesCount[0] = 1;

  int nb = 2;
  IntegerVector validIndices {0, 1};
  IntegerVector::iterator i;

  // Boucle principale
  for (int t = 2; t <= n; t++)
  {

    valuesCumsum[t] =
      valuesCumsum[t - 1] + data[t - 1];

    // Premier indice en dur pour réaliser un PELT
    i = validIndices.begin();
    firstCost = costRecord[*i] - pow(valuesCumsum[t] - valuesCumsum[*i], 2) / (t - (*i));

    optimalCost = firstCost + penalty;
    optimalChangepoint = *i;

    // Boucle OP
    ++i;
    while (i != validIndices.end())
    {
      locCost = costRecord[*i] + penalty - pow(valuesCumsum[t] - valuesCumsum[*i], 2) / (t - (*i));
      if (locCost <= optimalCost)
      {
        optimalCost = locCost;
        optimalChangepoint = *i;
      }
      ++i;
    }

    costRecord[t] = optimalCost;
    changepointsForward[t] = optimalChangepoint;


    // Elagage
    IntegerVector newValidIndices;
    nb = 0;

    i = validIndices.begin();

    // PELT sur le premier indice
    if (firstCost <= optimalCost)
    {
      newValidIndices.push_back(*i);
      nb++;
    }

    // DUSTvector sur le reste
    j = *i;
    ++i;
    while (i != validIndices.end())
    {
      testValue = simpleTestVector(t, *i, j, valuesCumsum, costRecord);
      if (testValue < optimalCost)
      {
        newValidIndices.push_back(*i);
        nb++;
      }
      j = validIndices[floor(nb * Rcpp::runif(1)[0])];
      ++i;
    }
    validIndices = newValidIndices;


    validIndices.push_back(t);
    nb++;
    indicesCount[t - 1] = nb;
  }

  // Backtrack des changepoints
  IntegerVector changepoints {n};
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
