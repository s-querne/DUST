
######################################################
#############    retrieve_changepoints   #############
######################################################


#' retrieve_changepoints
#'
#' @description builds the optimal changepoints by backtracking the optimal last changepoint path
#' @param backtrack_changepoints a vector of indices. a path must lead from the last value to 0
#' @return a vector of all optimal changepoints at time n = length(input)
#' @examples
#' example_backtrack = c(0, 0, 0, 2, 3, 4, 0, 2)
#' retrieve_changepoints(example_backtrack)
#'
#' example_backtrack2 = c(0, 0, 0, 2, 2, 2, 0, 0)
#' retrieve_changepoints(example_backtrack2)
#'

retrieve_changepoints <- function(
    backtrack_changepoints
)
{

  n = length(backtrack_changepoints)

  ## Initializing loop ##
  changepoints = n
  current_backtrack = n

  ## Conditional loop ##
  repeat {

    ## Retrieve last changepoint ##
    next_backtrack = backtrack_changepoints[current_backtrack]

    ## Loop break condition ##
    if (next_backtrack == 0) break

    ## Updating the changepoints ##
    changepoints = c(next_backtrack, changepoints)

    ## Next step ##
    current_backtrack = next_backtrack
  }

  return(changepoints)

}

