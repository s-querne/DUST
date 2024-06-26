% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DUST_testFunctions.R
\name{random_simple_test}
\alias{random_simple_test}
\title{random_simple_test}
\usage{
random_simple_test(values_cumsum, cost_scale = 1, pruning = NULL)
}
\arguments{
\item{values_cumsum}{the cumulative sum of the data}

\item{cost_scale}{value of the factor applied to the cost function. defaults to 1}

\item{pruning}{a value in 0 = no pruning step, 1 = PELT pruning step, 2 = DUST pruning step, 3 = PELT -> DUST pruning steps}
}
\value{
the function that performs the test
}
\description{
Creates a function that performs the random dual lagrangian test in the guassian penalized changepoint detection model
}
