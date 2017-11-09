# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer
# Edit: Michael Graber

#' Return the height difference of the input dataframe withe the mean height of the dataframe
#'
#' Return the height difference of the input dataframe withe the mean height of the dataframe.
#' @param students.input a dataframe with columns: names, heigth, sex
#' @param sex.specific a boolean which indicates if the height difference is calculated sex specific
#' @return A dataframe with column names and mean height difference
#' @author Jann Goschenhofer, Michael Graber
#' @export
#' @importFrom magrittr %>%
#' @example checkHeight(students)

checkHeight = function(students.input = students, sex.specific=TRUE){

  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")

  male.mean = students.input[,"height"] %>% mean
  female.mean = students.input[,"height"] %>% mean

  for (i in 1:nrow(students.input)) {
    # calculate sex-specific deviations from the mean
    if (students.input[i, "sex"] == "F") {
      height.diff = 100*(as.numeric(as.vector(students.input[i,]$height)) - female.mean)
    }
    else {
      height.diff = 100*(as.numeric(as.vector(students.input[i, ]$height)) - male.mean)
    }
    result.frame[i, "name"] = as.character(students.input[i, "name"])
    result.frame[i, "difference"] = height.diff
  }
  return(result.frame)
}
