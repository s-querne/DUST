
######################################################
#############    polynomial_parameters   #############
######################################################

#' polynomial_parameters
#'
#' @description Computes the parameters of the input values' Gaussian cost polynomial's parameters
#' @param values_cumsum a vector or single value containing the data points that constitute the polynomial
#' @param constant a value that vertically shifts the polynomial's values
#' @param cost_scale value of the factor applied to the cost function. defaults to 1
#' @return a list containing the a, b, and c, parameters corresponding, respectively, to the quadratic, linear, and constant, term of the polynomial
#' @examples

polynomial_parameters <- function(
    loc,
    loc_current,
    values_cumsum,
    cost_scale = 1
)
{
  ############
  ### STOP ###
  ############

  if(!is.numeric(values_cumsum)){stop('values are not all numeric')}

  slice_sum = slice_cumsum(values_cumsum, loc, loc_current)

  return(list(
    a = 2 * cost_scale * (loc_current - loc),
    b = - 2 * cost_scale * slice_sum
  ))
}

################################################
#############    dual_evaluation   #############
################################################

#' dual_evaluation
#'
#' @description Computes the value of the lagrangian dual function at input point(s) with input parameters
#' @param mu the point at which the lagrangian dual function is computed
#' @param a1 the quadratic parameter of the objective polynomial
#' @param b1 the linear parameter of the objective polynomial
#' @param c1 the constant parameter of the objective polynomial
#' @param a2 the quadratic parameter of the constraint polynomial
#' @param b2 the linear parameter of the constraint polynomial
#' @param c2 the constant parameter of the constraint polynomial
#' @return the computed value
#' @examples



dual_evaluation <- function(
    mu,
    a1,
    b1,
    c1,
    a2,
    b2,
    c2
)
{
  ############
  ### STOP ###
  ############

  if(!is.numeric(mu)) stop('computed points are not all numeric')
  parameters = c(a1, b1, c1, a2, b2, c2)
  parameters_print = c('a1', 'b1', 'c1', 'a2', 'b2', 'c2')
  if(!is.numeric(parameters)) {
    error_parameters = parameters_print[which(!is.numeric(parameters))]
    stop(paste('at least one parameter is not numeric. these parameters are:', error_parameters, collapse = ', '))
  }
  if(any(sapply(parameters, length) > 1)) {
    error_parameters = parameters_print[which(sapply(parameters, length) > 1)]
    stop(paste('at least one parameter has length greater than 1:', error_parameters, collapse = ', '))
  }
  if(a1 == a2) stop('invalid parameters: a1 and a2 cannot have equal values')

  numerator = -(b1 + mu * (b1 - b2))^2
  denominator = 2 * (a1 + mu * (a1 - a2))
  constant = c1 + mu * (c1 - c2)

  return(numerator / denominator + constant)
}

###########################################
#############    dual_roots   #############
###########################################

#' dual_roots
#'
#' @description Computes the values of the roots of the lagrangian dual function's derivative
#' @param a1 the quadratic parameter of the objective polynomial
#' @param b1 the linear parameter of the objective polynomial
#' @param c1 the constant parameter of the objective polynomial
#' @param a2 the quadratic parameter of the constraint polynomial
#' @param b2 the linear parameter of the constraint polynomial
#' @param c2 the constant parameter of the constraint polynomial
#' @return the computed roots
#' @examples
#'

dual_roots <- function(
    a1,
    b1,
    c1,
    a2,
    b2,
    c2
)
{
  ############
  ### STOP ###
  ############

  parameters = c(a1, b1, c1, a2, b2, c2)
  parameters_print = c('a1', 'b1', 'c1', 'a2', 'b2', 'c2')
  if(!is.numeric(parameters)) {
    error_parameters = parameters_print[which(!is.numeric(parameters))]
    stop(paste('at least one parameter is not numeric. these parameters are:', error_parameters, collapse = ', '))
  }
  if(any(sapply(parameters, length) > 1)) {
    error_parameters = parameters_print[which(sapply(parameters, length) > 1)]
    stop(paste('at least one parameter has length greater than 1:', error_parameters, collapse = ', '))
  }
  if(a1 == a2) stop('invalid parameters: a1 and a2 cannot have equal values')

  roots = rep(NA, 2)

  A = (a1 - a2) * (b1 - b2)^2 - 2 * (a1 - a2)^2 * (c1 - c2)
  B = 2 * a1 * ((b1 - b2)^2 - 2 * (a1 - a2) * (c1 - c2))
  C = 2 * a1 * b1 * (b1 - b2) - b1^2 * (a1 - a2) - 2 * a1^2 * (c1 - c2)
  discriminant = B^2 - 4 * A * C

  if (discriminant > 0) {
    roots[1] = (-B + sqrt(discriminant)) / (2 * A)
    roots[2] = (-B - sqrt(discriminant)) / (2 * A)
  }
  else if (discriminant == 0) {
    roots[1] = -B / (2 * A)
  }

  return(roots)
}
