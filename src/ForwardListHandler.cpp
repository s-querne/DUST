#include <Rcpp.h>
#include "ForwardListHandler.h"
#include <forward_list>
#include <vector>

using namespace Rcpp;

ForwardListHandler::ForwardListHandler(double sampleSize) : sampleSize(sampleSize) {
  sampleSize = pow(sampleSize, 1.25);
  reset_uniform();
}

ForwardListHandler::~ForwardListHandler() {}

void ForwardListHandler::add(int value) {
  list.push_front(value);
  pointers.push_back(&list.front());
  length++;
}

void ForwardListHandler::next() {
  ++current;
}

void ForwardListHandler::next_prune() {
  before = current;
  ++current;
  ++pointersCurrent;
  lengthConstraints--;
}

void ForwardListHandler::reset() {
  current = list.begin();
}

void ForwardListHandler::reset_prune() {
  lengthConstraints = length - 1;
  current = list.begin();
  before = list.before_begin();
  pointersCurrent = pointers.rbegin();
}

void ForwardListHandler::reset_uniform() {
  randomU = Rcpp::runif(sampleSize);
  u = randomU.begin();
}

void ForwardListHandler::prune() {
  current = list.erase_after(before);
  pointersCurrent = std::vector<int*>::reverse_iterator(pointers.erase(std::next(pointersCurrent).base()));
  length--;
  lengthConstraints--;
}

int ForwardListHandler::read() {
  return *current;
}

int ForwardListHandler::debug_read() {
  if (!check()) return -1;
  return *current;
}

int* ForwardListHandler::draw() {
  int* output = pointers[floor(lengthConstraints * (*u))];
  ++u;
  if (u == randomU.end())
  {
    reset_uniform();
  }
  return output;
}

bool ForwardListHandler::check() {
  if (current == list.end()) {return false;}
  return true;
}

bool ForwardListHandler::check_prune() {
  // if (length <= 1 || lengthConstraints <= 0) {return false;}
  if (lengthConstraints <= 0) {return false;}
  return true;
}

std::forward_list<int> ForwardListHandler::get_list() {
  return list;
}


RCPP_MODULE(ForwardListHandlerModule) {
  class_<ForwardListHandler>("ForwardListHandler")
  .constructor()
  .method("add", &ForwardListHandler::add)
  .method("next", &ForwardListHandler::next)
  .method("next_prune", &ForwardListHandler::next_prune)
  .method("reset", &ForwardListHandler::reset)
  .method("reset_prune", &ForwardListHandler::reset_prune)
  .method("reset_uniform", &ForwardListHandler::reset_uniform)
  .method("check", &ForwardListHandler::check)
  .method("check_prune", &ForwardListHandler::check_prune)
  .method("prune", &ForwardListHandler::prune)
  .method("read", &ForwardListHandler::read)
  .method("debug_read", &ForwardListHandler::debug_read)
  .method("draw", &ForwardListHandler::draw)
  .method("get_list", &ForwardListHandler::get_list)
  ;
}
