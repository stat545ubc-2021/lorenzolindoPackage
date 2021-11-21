#' Summary Statistics Function
#'
#' This summary will calculate the following summary statistics for a given column in a
#' dataset of interest: mean, median, max, min, standard deviation.
#'
#' @param dataset_of_interest The dataset of interest that the user wants to analyze.
#' This parameter was named this way so that it is very clear to the user what the argument should be.
#' @param col_of_interest The column of interest that want to calculate summary statistics for.
#' This parameter was named this way so that it is very clear to the user what the argument should be.
#' @param export_results Logical argument, where the default argument is FALSE. If the user inputs TRUE,
#' a csv containing the calculated summary statiatics is created within the main directory. This parameter was
#' named this way so that the user can determine whether or not they want to export results to a csv.
#'
#' @import tidyverse datateachr
#'
#' @return A data frame that contains the summary statistics. If the user specifies TRUE for the third argument
#' (export_results), then the function should also create a csv file.
#'
#' @examples
#' summaryStats(penguins, bill_length_mm)
#' summaryStats(mtcars, hp)
#' summaryStats(vancouver_trees, height_range_id)
#'
#' @export

summaryStats <- function(dataset_of_interest, col_of_interest, export_results = FALSE) {

  if(!is.data.frame(dataset_of_interest)) {
    stop("Error - inputted first argument (dataset_of_interest) is not a dataframe. Argument is: ", class(dataset_of_interest))
  }

  calculated_stats <- dataset_of_interest %>%
    summarise(mean = mean({{col_of_interest}}, na.rm = TRUE),
              median = median({{col_of_interest}}, na.rm = TRUE),
              max = max({{col_of_interest}}, na.rm = TRUE),
              min = min({{col_of_interest}}, na.rm = TRUE),
              sdev = sd({{col_of_interest}}, na.rm = TRUE))

  if(export_results == TRUE) {
    write.csv(calculated_stats, "results.csv")
  }

  return(calculated_stats)
}
