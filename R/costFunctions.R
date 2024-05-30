
###########################################
#############    gauss_cost   #############
###########################################


#' gauss_cost
#'
#' @description Computes the variable part of the L2 cost with optimal parameter on given data
#' @param loc first index of the sum
#' @param loc_current last index of the sum
#' @param values_cumsum cumulative sum of the data
#' @return the linear and quadratic terms of the L2 cost function with optimal parameter
#' @examples

gauss_cost <- function(
    loc,
    loc_current,
    values_cumsum
)
{
  ############
  ### STOP ###
  ############

  if(!is.numeric(values_cumsum)){stop('values are not all numeric')}

  ############  function enclosure   ############

  slice_length = loc_current - loc

  slice_sum = slice_cumsum(values_cumsum, loc, loc_current)
  argmin = slice_sum / slice_length

  linear = - 2 * slice_sum * argmin
  quadratic = slice_length * argmin^2

  return(linear + quadratic)

}

