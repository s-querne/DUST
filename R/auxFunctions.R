#############################################
#############    slice_cumsum   #############
#############################################

#' generate_random_changepoints
#'
#' @description Retrieves the sum between two values by using the data's cumulative sum
#' @param csum a vector of the cumulative sum of the data
#' @param loc lower (excluded) bound of the range over which the sum is performed
#' @param loc_current upper (included) bound of the range over which the sum is performed
#' @return the sum of the data between the (loc+1)-th element and the (loc_current)-th element
#' @examples
#' generate_random_changepoints(1e3, 1e3/50, 20, 1, 9)
#' generate_random_changepoints(1e2, 0, 20, 1, 9)


#
# Returns sum of y between some loc and loc_c
# urrent using cumsum vector csum = cumsum(y)
#

slice_cumsum = function(csum, loc, loc_current) {
  if (loc > 0) csum[loc_current] - csum[loc]
  else csum[loc_current]
}
