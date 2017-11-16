# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Tobias Riebe
# Edit: Michael Graber

#' Return the height difference of the input dataframe withe the mean height of the dataframe
#'
#' Return the height difference of the input dataframe withe the mean height of the dataframe.
#' @param students.input a dataframe with columns: names, heigth, sex
#' @param sex.specific a boolean which indicates if the height difference is calculated sex specific
#' @param print.statement a boolean; TRUE prints a message after calculation is done
#' @return A dataframe with column names and mean height difference
#' @author Tobias Riebe, Michael Graber
#' @export
#' @importFrom magrittr %>%
#' @import checkmate

checkHeight <- function(students.input, sex.specific = TRUE, print.statement = FALSE){
# Check input parameter
  assertLogical(sex.specific)
  assertLogical(print.statement)
  assertDataFrame(students.input, types = c("numeric", "numeric", "numeric", "factor", "character"))
  assertNumeric(students.input[,"height"], lower = 1.3, upper = 2.4)
  assertFactor(students.input[,"sex"], levels = c("M", "F"))

  #Check if the sex specific height difference or the difference to the whole population should be calculated
  if(sex.specific == TRUE){
    #Calculate the gender specific means
    women_mean_height = students.input[,"height"][students.input[,"sex"] == "F"] %>%
      mean %>% as.numeric
    men_mean_height = students.input[,"height"][students.input[,"sex"] == "M"] %>%
      mean %>% as.numeric
    #apply a function to the rows of the input dataframe
    height_vector = apply(students.input, MARGIN = 1,
                          FUN = function(student){
                            #substract the gender specific means from the individuals to get height differnces
                            (if (student["sex"] == "M") men_mean_height - as.numeric(student["height"])
                             else women_mean_height - as.numeric(student["height"]) )
                          } )
    #create the final dataframe containing name od the students and the height differnces
    #multiple height differences by 100 to get values in cm
    result.frame = data.frame("name" = students.input$name, "sexspec_height_diff" = height_vector*100)
  } else{
    #calculate the mean height of the whole population
    mean_height = students.input[,"height"] %>% mean %>% as.numeric
    #apply a function to the rows of the input dataframe
    height_vector = apply(students.input, MARGIN = 1,
                          FUN = function(student){
                            #substract the gender specific means from the individuals to get height differnces
                            mean_height - as.numeric(student["height"])
                          } )
    #create the final dataframe containing name od the students and the height differnces
    #multiple height differences by 100 to get values in cm
    result.frame = data.frame("name" = students.input$name, "height_diff" = height_vector*100)
  }
  #return the dataframe
  return(result.frame)
  # Print a message
  if(print.statement == TRUE){
    print("Yippie, I calculated the mean differences!")
  }
}
