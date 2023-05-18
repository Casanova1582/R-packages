#' Chris's log_summed_exps function
#'
#' @param x A vector of type numeric
#'
#' @return The log of the sum of the elements exponentiated.
#'
#' @author Chris Chen
#'
#' @examples
#' log_summed_exps(1:2000)
#' log_summed_exps(20)
#'
#' @importFrom randomForest randomForest importance varImpPlot
#'
#' @export
log_summed_exps = function(x) {
  if (is.numeric(x) == FALSE) {
    print("x needs to be a numeric vector")
    stop()
  }
  n = length(x)
  if (n == 1) {
    return(x)
  } else {
    sum = 0
    for (i in 2:(n-1)) {
      sum = sum + exp(x[i]-x[n])
    }
    return(x[n] + log(1 + sum))
  }
}
