#' Calculate a trimmed standard deviation
#'
#' @param x numeric vector whose sample standard deviation is wanted,
#' or an object of a class for which a method has been defined
#' (see also ‘details’). `NA` and NaN values are not allowed in
#' numeric vectors unless na.rm is TRUE.
#' @param na.rm
#' @param probs
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
trimSD = function(x, na.rm = FALSE, probs = c(0.05, 0.95), ...){
  ## make sure that the probabilities are in ascending order
  probs = sort(probs)

  if(length(probs) <= 2){
    stop("You need to specify two probabilities")
  }

  if(probs[1] <= 0 || probs[1] >= 0.5){
    stop("The lower probability must greater than 0 and less than 0.5")
  }

  if(probs[2] <= 0.5 || probs[1] >= 1){
    stop("The upper probability must greater than 0.5 and less than 1")
  }

  nx = length(x)

  if(nx < 10){
    warning("Using this function on fewer than 10 observations is unstable\nand probably not sensible")
  }


  if(probs[2] - probs[1] > 0.5){
    warning("Are you sure you want to discard this much data?")
  }

  qx = quantiles(x, na.rm = na.rm, probs = probs, ... = ...)

  newx = x[x >= qx[1] & x <= qx[2]]
  if(length(newx) < 2){
    stop("The trimmed vector has insufficient data to calculate a standard deviation"
  }elseif(length(newx) <= 5){
    warning("You are calculating the standard deviation of 5 or fewer observations")
  }

  sx = sd(newx, na.rm = na.rm)
}
