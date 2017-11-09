library(dplyr)


age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

write.csv(students, file = "data-raw/students.csv")
devtools::use_data(students, overwrite = TRUE)
