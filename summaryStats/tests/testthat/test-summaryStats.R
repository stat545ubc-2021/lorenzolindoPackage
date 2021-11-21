suppressMessages( {
  library(dplyr)
  library(palmerpenguins)
  library(datateachr)
  library(gapminder)
})

test_that("The first argument (dataset_of_interest) is a dataframe", {
  expect_true(is.data.frame(penguins))
})

test_that("The output of the function is a dataframe", {
  expect_true(is.data.frame(summaryStats(vancouver_trees, height_range_id)))
})

test_that("Error occurs when a non-dataframe is entered as the first argument (dataset_of_interest)", {
  expect_error(summaryStats(5, gdpPercap))
})

test_that("An output file titled 'results.csv' is created in the main directory if TRUE is inputted in the third argument", {
  summaryStats(mtcars, hp, TRUE)
  expect_true(file.exists("results.csv"))
})

# This code was used because the output test file still existed even after running rm("results.csv").
file.remove("results.csv")
