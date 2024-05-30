
##################################################
#############    DUST_partitioning   #############
##################################################

#' DUST_partitioning
#'
#' @description Computes the exact optimal partitioning for given data, cost function and cost penalization with pruning steps option (DUST as default)
#' @param values a vector containing the data points
#' @param penalty the value of the changepoint cost penalty
#' @param cost_function the cost function to be used when computing the cost of the model. defaults to gauss_cost
#' @param cost_scale value of the factor applied to the cost function. defaults to 1
#' @param pruning a value in 0 = no pruning step, 1 = PELT pruning step, 2 = DUST pruning step, 3 = PELT -> DUST pruning steps
#' @param test_function the test function to be used when performing the DUST pruning step. defaults to simple_test
#' @return a list containing a vector of the optimal cost at each step and a vector containing the optimal model on the full data
#' @examples

DUST_partitioning <- function(
    values,
    penalty = 2 * log(length(values)), ### INTEGRER UN ESTIMATEUR DE SIGMA DANS LA PENALISATION (MAD, HALL)
    cost_function = gauss_cost,
    cost_scale = 1,
    pruning = 2,
    test_function = simple_test
)
{
  ############
  ### STOP ###
  ############

  if(!is.numeric(values)){stop('values are not all numeric')}
  if(!is.numeric(penalty)){stop('penalty is not numeric')}
  if(length(penalty) != 1){stop(paste('penalty length should be 1. currently', length(penalty)))}
  if(penalty < 0) warning('negative penalty given')
  if(!is.numeric(cost_scale)) stop('cost scale is not numeric')
  if(cost_scale < 0) warning('negative cost scale given')
  if(!(pruning %in% 0:3)) stop('invalid pruning value given : pruning value should be 0, 1, 2, or 3')


  #########################
  ### DUST PARTITIONING ###
  #########################

  n = length(values)


  ############  cumsum initialization   ############

  values_cumsum = cumsum(values)


  ############  record and backtrack initialization   ############

  cost_record = c(-penalty, numeric(n)) # Contient les optimal_cost
  backtrack_changepoints = c(0, numeric(n - 1))


  ############  unpruned indices initialization   ############

  valid_index = 0
  nb_record = c(1, numeric(n))


  ############  pruning step initialization   ############

  pruning_instance = pruning_step(values_cumsum, pruning, test_function, cost_scale)


  ############  main loop   ############

  for (loc_current in 1:n) {

    ############ OP step ############

    constant = cost_record[valid_index + 1]
    model_cost = constant + cost_scale * vapply(valid_index, cost_function, numeric(1), loc_current=loc_current, values_cumsum=values_cumsum)

    min_index = which.min(model_cost)
    optimal_cost = model_cost[min_index] + penalty
    new_changepoint = valid_index[min_index]

    ## Saving the optimal model and associated cost ##
    backtrack_changepoints[loc_current + 1] = new_changepoint
    cost_record[loc_current + 1] = optimal_cost


    ############ Pruning step ############

    if (pruning) {
      test_treshold = optimal_cost
      valid_index = pruning_instance(loc_current, valid_index, model_cost, cost_record, test_treshold)
    }


    ############ Valid index update ############

    valid_index = c(valid_index, loc_current)
    nb_record[loc_current + 1] = length(valid_index)

  }


  ############ Backtracking step ############

  changepoints = n
  current_backtrack = n
  repeat {
    next_backtrack = backtrack_changepoints[current_backtrack]
    if (next_backtrack == 0) break
    changepoints = c(next_backtrack, changepoints)
    current_backtrack = next_backtrack
  }

  return(list(changepoints = changepoints, nb = nb_record, lastIndexSet = valid_index, costQ = cost_record[-1]))
}
