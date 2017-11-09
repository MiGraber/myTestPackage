#' Calculates the arithmetic mean
#' 
#' Calculates the arithmetic mean and round on the 3rd digit
#' @param x a numeric vector
#' @return an integer
#' @author Michael Graber
#' 
mean = function(x){
  round(sum(x)/length(x),3)
}