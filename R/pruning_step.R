
#############################################
#############    pruning_step   #############
#############################################

#' pruning_step
#'
#' @description Performs the pruning step over the data with given test function
#' @param values_cumsum a vector containing the data points
#' @param pruning a value in 0 = no pruning step, 1 = PELT pruning step, 2 = DUST pruning step, 3 = PELT -> DUST pruning steps
#' @param test_function the test function to be used when performing the DUST pruning step. defaults to simple_test
#' @param cost_scale value of the factor applied to the cost function. defaults to 1
#' @return a vector of the indices kept after the pruning step
#' @examples

pruning_step <- function(
    values_cumsum,
    pruning = 2,
    test_function = simple_test,
    cost_scale = 1
)
{

  return(
    function(loc_current, valid_index, model_cost, cost_record, test_treshold = 0) {


      ############  Initializing index  ############

      new_index = NULL


      ############  Loop over input indices  ############

      for (objective_index in seq_along(valid_index)) {


        ############  PELT step  ############

        if (pruning %in% c(1, 3))
          if (model_cost[objective_index] > test_treshold) next # skips DUST step if PELT steps results in pruning of objective_index


        ############  DUST step  ############

        if (pruning %in% c(2, 3)){
          if (objective_index > 1) {

            ## Drawing a random constraint valid_index ##
            constraint_index = sample(objective_index - 1, 1)

            ## Performing the input test on the set locations ##
            loc_objective = valid_index[objective_index]
            loc_constraint = valid_index[constraint_index]

            #print(c(oi=objective_index, ol=loc_objective, ci=constraint_index, cl=loc_constraint))
            test_result = test_instance(loc_current, loc_objective, loc_constraint, cost_record, test_treshold)

            if (test_result) next # skips to next iteration if DUST steps results in pruning of objective_index
          }}


        ############  Updating the new valid index if objective_index fails the pruning test(s)  ############

        new_index = c(new_index, valid_index[objective_index])

      }

      return(new_index)

    }
  )
}
