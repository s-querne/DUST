// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// DUSTclass
List DUSTclass(NumericVector data, double penalty);
RcppExport SEXP _DUSTpartitioning_DUSTclass(SEXP dataSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP);
    rcpp_result_gen = Rcpp::wrap(DUSTclass(data, penalty));
    return rcpp_result_gen;
END_RCPP
}
// OP
List OP(NumericVector data, double penalty);
RcppExport SEXP _DUSTpartitioning_OP(SEXP dataSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP);
    rcpp_result_gen = Rcpp::wrap(OP(data, penalty));
    return rcpp_result_gen;
END_RCPP
}
// PELT
List PELT(NumericVector data, double penalty);
RcppExport SEXP _DUSTpartitioning_PELT(SEXP dataSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP);
    rcpp_result_gen = Rcpp::wrap(PELT(data, penalty));
    return rcpp_result_gen;
END_RCPP
}
// simpleTest
double simpleTest(int t, int i, int j, const NumericVector& valuesCumsum, const NumericVector& costRecord);
RcppExport SEXP _DUSTpartitioning_simpleTest(SEXP tSEXP, SEXP iSEXP, SEXP jSEXP, SEXP valuesCumsumSEXP, SEXP costRecordSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type t(tSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type valuesCumsum(valuesCumsumSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type costRecord(costRecordSEXP);
    rcpp_result_gen = Rcpp::wrap(simpleTest(t, i, j, valuesCumsum, costRecord));
    return rcpp_result_gen;
END_RCPP
}
// explo
List explo();
RcppExport SEXP _DUSTpartitioning_explo() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(explo());
    return rcpp_result_gen;
END_RCPP
}
// generate_runif1
void generate_runif1(int n);
RcppExport SEXP _DUSTpartitioning_generate_runif1(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    generate_runif1(n);
    return R_NilValue;
END_RCPP
}
// generate_runif2
void generate_runif2(int n);
RcppExport SEXP _DUSTpartitioning_generate_runif2(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    generate_runif2(n);
    return R_NilValue;
END_RCPP
}
// logging
void logging(std::string txt);
RcppExport SEXP _DUSTpartitioning_logging(SEXP txtSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type txt(txtSEXP);
    logging(txt);
    return R_NilValue;
END_RCPP
}
// resetLogging
void resetLogging(String path);
RcppExport SEXP _DUSTpartitioning_resetLogging(SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type path(pathSEXP);
    resetLogging(path);
    return R_NilValue;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_ForwardListHandlerModule();

static const R_CallMethodDef CallEntries[] = {
    {"_DUSTpartitioning_DUSTclass", (DL_FUNC) &_DUSTpartitioning_DUSTclass, 2},
    {"_DUSTpartitioning_OP", (DL_FUNC) &_DUSTpartitioning_OP, 2},
    {"_DUSTpartitioning_PELT", (DL_FUNC) &_DUSTpartitioning_PELT, 2},
    {"_DUSTpartitioning_simpleTest", (DL_FUNC) &_DUSTpartitioning_simpleTest, 5},
    {"_DUSTpartitioning_explo", (DL_FUNC) &_DUSTpartitioning_explo, 0},
    {"_DUSTpartitioning_generate_runif1", (DL_FUNC) &_DUSTpartitioning_generate_runif1, 1},
    {"_DUSTpartitioning_generate_runif2", (DL_FUNC) &_DUSTpartitioning_generate_runif2, 1},
    {"_DUSTpartitioning_logging", (DL_FUNC) &_DUSTpartitioning_logging, 1},
    {"_DUSTpartitioning_resetLogging", (DL_FUNC) &_DUSTpartitioning_resetLogging, 1},
    {"_rcpp_module_boot_ForwardListHandlerModule", (DL_FUNC) &_rcpp_module_boot_ForwardListHandlerModule, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_DUSTpartitioning(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
