#include<Rcpp.h>
#include<forward_list>
#include<cmath>
#include<random>
#include<limits>
using namespace Rcpp;

// [[Rcpp::export]]
List PELTcpp(NumericVector data, double penalty = 0) {
  const int n = data.size();

  if(penalty == 0)
    penalty = 2 * log(n);

  std::vector<double> valueCumsum(n + 1, 0.0), costRecord(n + 1, - penalty), modelCost(n);
  std::vector<int> changepointsForward(n + 1, 0), indicesCount(n);
  std::forward_list<int> validIndices {0};

  double tempMin, locSum, locCost, argMin, testThreshold;
  int tempChangepoint, locLength, nb;

  for (int locCurrent = 1; locCurrent <= n; locCurrent++)
  {
    valueCumsum[locCurrent] =
      valueCumsum[locCurrent - 1] + data[locCurrent - 1];

    tempMin = std::numeric_limits<double>::infinity();

    for (int loc: validIndices)
    {
      locCost = costRecord[loc];
      locSum = valueCumsum[locCurrent] - valueCumsum[loc];
      locLength = locCurrent - loc;
      argMin = locSum / locLength;
      locCost += locLength * pow(argMin, 2) - 2 * locSum * argMin;
      modelCost[loc] = locCost;

      if (locCost < tempMin)
      {
        tempMin = locCost;
        tempChangepoint = loc;
      }
    }

    testThreshold = tempMin + penalty;
    costRecord[locCurrent] = testThreshold;
    changepointsForward[locCurrent] = tempChangepoint;

    std::forward_list<int>::iterator before = validIndices.before_begin();
    std::forward_list<int>::iterator locObjective;

    nb = 0;
    for (
      locObjective = validIndices.begin();
      locObjective != validIndices.end();
    )
    {
      if (modelCost[*locObjective] > testThreshold) {
        locObjective = validIndices.erase_after(before);
      }
      else {
        before = locObjective;
        ++locObjective;
        ++nb;
      }
    }

    validIndices.push_front(locCurrent);
    indicesCount.push_back(nb);
  }

  NumericVector changepoints = NumericVector::create(n);
  int newChangepoint = changepointsForward[n];
  while (newChangepoint != 0) {
    changepoints.push_front(newChangepoint);
    newChangepoint = changepointsForward[newChangepoint];
  }

  List output;
  output["changepoints"] = changepoints;
  output["nb"] = indicesCount;
  output["lastIndexSet"] = validIndices;
  output["costQ"] = costRecord;

  return output;
}

/*** R
n = 1e2
y = rnorm(n)
# system.time(cp1 <- PELTcpp(y), gcFirst = TRUE)
# system.time(cp2 <- DUST_partitioning(y), gcFirst = TRUE)
microbenchmark::microbenchmark(PELTcpp(y), DUST_partitioning(y, pruning=1), times = 20)
*/

