#' Calculates the arithmetic mean
#'
#' Calculates the arithmetic mean and round on the 3rd digit
#' @param x a numeric vector
#' @return an integer
#' @author Michael Graber
#'
#' @import checkmate
mean = function(x, digits = 3L){
  assert(checkInteger(x),
         checkNumber(x),
         checkVector(x),
         combine = "or")
  assertInt(digits)
  round(sum(x)/length(x), digits)
}
