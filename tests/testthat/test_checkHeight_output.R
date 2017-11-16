library(myTestPackage)
context("checkHeight output")

students.subset1 = students[1,]
students.subset2 = students[1:5,]
students.subset3 = students[-4,]
test_that("checkHeight outputs correct df dimension", {
  expect_data_frame(checkHeight(students.subset1), nrows = nrow(students.subset1))
  expect_data_frame(checkHeight(students.subset2), nrows = nrow(students.subset2))
  expect_data_frame(checkHeight(students.subset3), nrows = nrow(students.subset3))

})

students.same.height = students
students.same.height$height = 1.89
test_that("checkHeight sex.specific arguments has the same output", {
  expect_equal(checkHeight(students.same.height, sex.specific = TRUE),
               checkHeight(students.same.height, sex.specific = FALSE) )
})


students.no.name = students
students.no.name$name = NULL
students.no.height = students
students.no.height$height = NULL
test_that("throw error if colums are missing in df", {
  expect_error(checkHeight(students.no.height), "undefined columns selected")
  expect_error(checkHeight(students.no.name), "arguments imply differing number of rows: 0, 8")
})


students.height.as.char = students
students.height.as.char$height = as.character(students.2$height)
students.sex.as.char = students
students.sex.as.char$sex = as.character(students.sex.as.char$sex)
students.too.tall = students
students.too.tall[1,"height"] = 5
students.too.small = students
students.too.small[5,"height"] = 0
test_that("argument checks", {
  expect_error(checkHeight(students, sex.specific = "A"),
               "Assertion on 'sex.specific' failed: Must be of type 'logical', not 'character'.")
  expect_error(checkHeight(students, print.statement = "A"),
               "Assertion on 'print.statement' failed: Must be of type 'logical', not 'character'.")
  expect_error(checkHeight(students.height.as.char), "failed: Must be of type 'numeric', not 'character'.")
  expect_error(checkHeight(students.sex.as.char), "failed: Must be of type 'factor', not 'character'.")
  expect_error(checkHeight(students.too.small), "failed: All elements must be >= 1.3.")
  expect_error(checkHeight(students.too.tall), "failed: All elements must be <= 2.4.")
})


test_that("print checking", {
  expect_output(checkHeight(students, print.statement = TRUE), "Yippie, I calculated the mean differences!")
})




