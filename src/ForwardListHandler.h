#ifndef FORWARDLISTHANDLER_H
#define FORWARDLISTHANDLER_H

#include <Rcpp.h>
#include <forward_list>
#include <vector>

using namespace Rcpp;

class ForwardListHandler {
public:
  ForwardListHandler(double sampleSize = 1e3);
  ~ForwardListHandler();
  void add(int value);
  
  void next();
  void next_prune();
  void reset();
  void reset_prune();
  void reset_uniform();
  bool check();
  bool check_prune();
  
  void prune();
  
  int* draw();
  int read();
  int debug_read();
  
  std::forward_list<int> get_list();
  
  int length = 0;
  int lengthConstraints;
  
private:
  double sampleSize;
  std::forward_list<int> list;
  std::vector<int*> pointers;
  std::forward_list<int>::iterator current;
  std::forward_list<int>::iterator before;
  std::vector<int*>::reverse_iterator pointersCurrent;
  NumericVector randomU;
  NumericVector::iterator u;
};

#endif
