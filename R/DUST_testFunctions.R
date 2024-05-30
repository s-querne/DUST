
############################################
#############    simple_test   #############
############################################

#' simple_test
#'
#' @description Performs the simple lagrangian dual test in the gaussian penalized changepoint detection model
#' @param values_cumsum the cumulative sum of the data
#' @param cost_scale value of the factor applied to the cost function. defaults to 1
#' @param pruning a value in 0 = no pruning step, 1 = PELT pruning step, 2 = DUST pruning step, 3 = PELT -> DUST pruning steps
#' @return the Boolean value of the test
#' @examples
#'

simple_test <- function(
    values_cumsum,
    cost_scale = 1,
    pruning = 2
)
{

  return(
    function(loc_current, loc_objective, loc_constraint, cost_record, test_treshold = 0) {

      ############
      ### STOP ###
      ############

      #print(c(loc_current=loc_current, loc_objective=loc_objective, loc_constraint=loc_constraint))

      if (!is.numeric(loc_current)) stop('loc_current is not numeric')
      if (!is.numeric(loc_objective)) stop('loc_objective is not numeric')
      if (!is.numeric(loc_constraint)) stop('loc_constraint is not numeric')
      if (!is.numeric(values_cumsum)) stop('values are not all numeric')
      if (!is.numeric(cost_record)) stop('cost_record are not all numeric')
      if (!is.numeric(test_treshold)) stop('test_treshold is not numeric')
      if (loc_current <= loc_objective) stop('loc_current should be greater than loc_objective')
      if (loc_objective <= loc_constraint) stop('loc_objective should be greater than loc_constraint')

      objective_slice = loc_current - loc_objective
      gap_slice = loc_objective - loc_constraint

      objective_sum = slice_cumsum(values_cumsum, loc_objective, loc_current)
      objective_mean = objective_sum / objective_slice
      gap_sum = slice_cumsum(values_cumsum, loc_constraint, loc_objective)
      gap_mean = gap_sum / gap_slice

      objective_cost = cost_record[loc_objective + 1]
      constraint_cost = cost_record[loc_constraint + 1]

      inv_scale = 1 / cost_scale

      cost_term = inv_scale * (objective_cost - constraint_cost)

      if (cost_term / gap_slice <= objective_mean^2 - 2 * objective_mean * gap_mean) {
        if (pruning == 3) return(FALSE)

        optimal_numerator = objective_sum
        optimal_denominator = objective_slice
        optimal_constant = objective_cost
      }
      else {
        mu_bound = objective_slice / gap_slice # t-i/i-j
        sqrt_cost = cost_term + gap_slice * gap_mean^2
        optimal_mu = mu_bound * (1 - sqrt(gap_slice) * abs(objective_mean - gap_mean) / sqrt(sqrt_cost))

        optimal_numerator = objective_sum - optimal_mu * gap_sum
        optimal_denominator = objective_slice - optimal_mu * gap_slice
        optimal_constant = objective_cost + optimal_mu * (objective_cost - constraint_cost)
      }

      test_value = - optimal_numerator^2 / (inv_scale * optimal_denominator) + optimal_constant

      return(test_value > test_treshold)

    }
  )
}

###################################################
#############    random_simple_test   #############
###################################################

#' random_simple_test
#'
#' @description Creates a function that performs the random dual lagrangian test in the guassian penalized changepoint detection model
#' @param values_cumsum the cumulative sum of the data
#' @param cost_scale value of the factor applied to the cost function. defaults to 1
#' @param pruning a value in 0 = no pruning step, 1 = PELT pruning step, 2 = DUST pruning step, 3 = PELT -> DUST pruning steps
#' @return the function that performs the test
#' @examples
#'

random_simple_test <- function(
    values_cumsum,
    cost_scale = 1,
    pruning = NULL
)
{

  return(
    function(loc_current, loc_objective, loc_constraint, cost_record, test_treshold = 0) {

      ############
      ### STOP ###
      ############

      if (!is.numeric(loc_current)) stop('loc_current is not numeric')
      if (!is.numeric(loc_objective)) stop('loc_objective is not numeric')
      if (!is.numeric(loc_constraint)) stop('loc_constraint is not numeric')
      #if (!is.numeric(values)) stop('values are not all numeric')
      if (!is.numeric(cost_record)) stop('cost_record are not all numeric')
      if (!is.numeric(test_treshold)) stop('test_treshold is not numeric')
      if (loc_current <= loc_objective) stop('loc_current should be greater than loc_objective')
      if (loc_objective <= loc_constraint) stop('loc_objective should be greater than loc_constraint')


      objective_slice = loc_current - loc_objective
      gap_slice = loc_objective - loc_constraint

      objective_sum = slice_cumsum(values_cumsum, loc_objective, loc_current)
      objective_mean = objective_sum / objective_slice
      gap_sum = slice_cumsum(values_cumsum, loc_constraint, loc_objective)
      gap_mean = gap_sum / gap_slice

      objective_cost = cost_record[loc_objective + 1]
      constraint_cost = cost_record[loc_constraint + 1]

      inv_scale = 1 / cost_scale

      mu_bound = objective_slice / gap_slice # t-i/i-j
      random_mu = runif(1, 0, mu_bound)

      optimal_numerator = objective_sum - random_mu * gap_sum
      optimal_denominator = objective_slice - random_mu * gap_slice
      optimal_constant = objective_cost + random_mu * (objective_cost - constraint_cost)

      test_value = - optimal_numerator^2 / (optimal_denominator * inv_scale) + optimal_constant

      return(test_value > test_treshold)

    }
  )
}
