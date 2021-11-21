#' Summary Statistics Function (summaryStats)
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
#' @import tidyverse datateachr palmerpenguins dplyr
#' @importFrom magrittr "%>%"
#' @importFrom stats "median"
#' @importFrom stats "sd"
#' @importFrom utils "write.csv"
#'
#' @return A data frame that contains the summary statistics. If the user specifies TRUE for the third argument
#' (export_results), then the function should also create a csv file.
#'
#' @examples
#' summaryStats(mtcars, hp)
#' summaryStats(iris, Sepal.Length)
#' summaryStats(PlantGrowth, weight)
#'
#' @export

summaryStats <- function(dataset_of_interest, col_of_interest, export_results = FALSE) {

  if(!is.data.frame(dataset_of_interest)) {
    stop("Error - inputted first argument (dataset_of_interest) is not a dataframe. Argument is: ", class(dataset_of_interest))
  }

  calculated_stats <- dataset_of_interest %>%
    dplyr::summarise(mean = mean({{col_of_interest}}, na.rm = TRUE),
              median = stats::median({{col_of_interest}}, na.rm = TRUE),
              max = max({{col_of_interest}}, na.rm = TRUE),
              min = min({{col_of_interest}}, na.rm = TRUE),
              sdev = stats::sd({{col_of_interest}}, na.rm = TRUE))

  if(export_results == TRUE) {
    utils::write.csv(calculated_stats, "results.csv")
  }

  return(calculated_stats)
}
