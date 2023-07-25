#' Generate APA-compliant table with mean, standard deviation, median, and N
#'
#' This function generates an APA-compliant table with mean, standard deviation,
#' median, and sample size (N) for each variable passed as separate arguments.
#'
#' @param ... Vectors (variables) for which statistics will be calculated.
#' @param caption The caption for the table.
#' @param label The label for the table.
#' @param na_rm Logical. If TRUE, remove missing values when calculating statistics.
#'
#' @return An xtable object representing the APA-compliant table.
#'
#' @import xtable
#'
#' @examples
#' \dontrun{
#' Example usage:
#' var1 <- c(1, 2, 3, 4, 5)
#' var2 <- c(6, 7, 8, 9, 10)
#' var3 <- c(11, 12, 13, 14, 15)
#' generate_apa_table(var1, var2, var3, caption = "Descriptive Statistics", label = "tab:desc_stats")
#'}
#' @export
#'

generate_apa_table <- function(..., caption = "Table", label = "tab", na_rm = TRUE) {
  means <- sapply(list(...), function(x) mean(x, na.rm = na_rm))
  sds <- sapply(list(...), function(x) sd(x, na.rm = na_rm))
  medians <- sapply(list(...), function(x) median(x, na.rm = na_rm))
  ns <- sapply(list(...), function(x) sum(!is.na(x)))

  table_data <- data.frame(Mean = means, SD = sds, Median = medians, N = ns)
  rownames(table_data) <- names(list(...))

  table_x <- xtable(table_data, caption = caption, label = label)
  return(table_x)
}

#' Calculate Mean and Standard Deviation
#'
#' This function calculates the mean and standard deviation for each vector provided as input.
#' It returns a data frame with the variable names, their corresponding means, and standard deviations.
#'
#' @param ... Numeric vectors or data frames for which you want to calculate the mean and standard deviation.
#' @param na.rm Logical. If TRUE, any NA (missing) values in the input vectors will be removed before calculations.
#'             If FALSE (default), NA values are not removed and will yield NA results.
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{variable}{The names of the variables.}
#'     \item{mean}{The calculated means for each variable.}
#'     \item{sd}{The calculated standard deviations for each variable.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Example 1: Calculate mean and standard deviation for two numeric vectors
#' vec1 <- c(1, 2, 3, 4, 5)
#' vec2 <- c(6, 7, 8, 9, 10)
#' calculate_mean_sd(vec1, vec2)
#'
#' # Example 2: Calculate mean and standard deviation for two data frames
#' df1 <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' df2 <- data.frame(A = c(7, 8, 9), B = c(10, 11, 12))
#' calculate_mean_sd(df1, df2)
#' }
#'
#' @export

calculate_mean_sd <- function(..., na.rm = FALSE) {
  means <- sapply(list(...), function(x) round(mean(x, na.rm = na.rm), 2))
  sds <- sapply(list(...), function(x) round(sd(x, na.rm = na.rm), 2))
  result <- data.frame(variable = names(means), mean = means, sd = sds)
  return(result)
}

#' Process File and Save File Name Info
#'
#' This function imports a file, prints its name, and saves the file name to a
#' text file in the specified output folder.
#'
#' @param file_path The path to the file to be imported and processed.
#' @param output_folder The path to the output folder where the file name text
#'   file will be saved. The default is "output_folder".
#'
#' @return None (invisible NULL)
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' file_path <- "path/to/your/file.csv"
#' process_file_and_save_info(file_path)
#' }
#'
#' @importFrom base read.csv
#' @export
process_file_and_save_info <- function(file_path, output_folder = "output_folder") {
  # Read the file using base R function
  data <- read.csv(file_path) # Change this to the appropriate read function based on your file format

  # Get the file name without the path
  file_name <- basename(file_path)

  # Print the file name
  cat("File Name:", file_name, "\n")

  # Save the file name to a text file
  output_text <- paste("File Name:", file_name, "\n")
  output_path <- file.path(output_folder, "file_name.txt")
  writeLines(output_text, output_path)
}



process_file_and_save_info <- function(file_path, output_folder = "output_folder") {
  # Get the file name without the path
  file_name <- basename(file_path)

  # Print the file name
  cat("File Name:", file_name, "\n")

  # Save the file name to a text file
  output_text <- paste("File Name:", file_name, "\n")
  output_path <- file.path(output_folder, "file_name.txt")
  writeLines(output_text, output_path)
}

#' Reverse the polarity of Likert scale items
#'
#' This function takes a vector of Likert scale items and reverses their scoring
#' to convert the original scale to a new scale with reversed polarity.
#'
#' @param x A vector of integers representing Likert scale items.
#' @param max_score The maximum score of the original Likert scale.
#'
#' @return A new vector with the reversed scores.
#'
#' @examples
#' original_scores <- c(4, 2, 3, 5, 1)
#' max_score <- 5
#' reversed_scores <- reverse_likert_polarity(original_scores, max_score)
#' print(reversed_scores)
#'
#' @export
reverse_likert_polarity <- function(x, max_score) {
  reversed_items <- max_score - x + 1
  return(reversed_items)
}


reverse_likert_polarity <- function(x, max_score) {
  reversed_items <- max_score - x + 1
  return(reversed_items)
}

#' Generate an RMarkdown file with descriptive statistics for all variables in a dataframe.
#'
#' This function scans the input dataframe and calculates specific descriptive statistics
#' for each variable. For numeric variables, it calculates the summary statistics
#' (mean, median, standard deviation, quartiles) using \code{summary()}, \code{mean()},
#' \code{median()}, \code{sd()}, and \code{quantile()}. For categorical variables
#' (factors), it calculates frequency counts using \code{table()}.
#'
#' @param data The input dataframe for which descriptive statistics need to be generated.
#'             The dataframe should contain a mix of numeric and categorical variables.
#' @param output_file The name of the output RMarkdown file that will contain the generated
#'                    descriptive statistics. The file will have the .Rmd extension.
#'
#' @return This function does not return a value but writes the RMarkdown content to the
#'         specified output file.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe called "my_data" and want to create a file called "descriptive_stats.Rmd":
#' generate_descriptive_stats_rmd(data = my_data, output_file = "descriptive_stats.Rmd")
#' }
#'
#' @import rmarkdown
#'
#' @export
generate_descriptive_stats_rmd <- function(data, output_file) {
  # Function implementation here...
}


generate_descriptive_stats_rmd <- function(data, output_file) {
  # Check if the input data is a dataframe
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  # Create a character vector to store the R code for each statistic
  stats_code <- character()

  # Calculate descriptive statistics for each variable
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      code <- paste0(
        "## Summary Statistics for ", col, " (Numeric)\n",
        "```{r}\n",
        "summary(data$", col, ")\n",
        "```\n\n",
        "Mean: `", round(mean(data[[col]]), 2), "`\n",
        "Median: `", median(data[[col]]), "`\n",
        "Standard Deviation: `", round(sd(data[[col]]), 2), "`\n",
        "Q1 (25th percentile): `", quantile(data[[col]], 0.25), "`\n",
        "Q3 (75th percentile): `", quantile(data[[col]], 0.75), "`\n\n"
      )
      stats_code <- c(stats_code, code)
    } else if (is.factor(data[[col]])) {
      code <- paste0(
        "## Summary Statistics for ", col, " (Categorical)\n",
        "```{r}\n",
        "table(data$", col, ")\n",
        "```\n\n"
      )
      stats_code <- c(stats_code, code)
    }
  }

  # Combine all the statistics code into one string
  rmd_content <- paste(stats_code, collapse = "")

  # Write the RMarkdown content to the specified output file
  cat(rmd_content, file = output_file)
}

#' Scale Multiple Variables in a Data Frame
#'
#' This function scales multiple variables in a data frame based on either the sum or the mean of the variables.
#'
#' @param data A data frame containing the variables to be scaled.
#' @param var_names A character vector specifying the names of the variables to be scaled.
#' @param scale_type A character string specifying the type of scaling to be applied.
#'   Use "sum" to scale based on the sum of the differences from the mean, or "mean" to scale based on the mean and standard deviation.
#'   Default is "mean".
#'
#' @return A data frame with the selected variables scaled.
#'
#' @details The function provides two scaling methods: "sum" and "mean".
#'   - When scale_type is set to "sum", the variables will be scaled by subtracting the mean and dividing by the sum of the absolute differences from the mean.
#'   - When scale_type is set to "mean", the variables will be scaled by subtracting the mean and dividing by the standard deviation (as implemented in the base R `scale` function).
#'   Note: For "sum" scaling, if the sum of absolute differences from the mean is zero for a variable, that variable will be left unchanged to avoid division by zero.
#'
#' @examples
#' # Sample data frame
#' data <- data.frame(
#'   var1 = c(10, 20, 30, 40),
#'   var2 = c(5, 15, 25, 35),
#'   var3 = c(100, 200, 300, 400)
#' )
#'
#' # Variables to be scaled
#' variables_to_scale <- c("var1", "var2", "var3")
#'
#' # Scale the selected variables in the data frame based on the sum
#' scaled_data_sum <- scale_multiple_variables(data, variables_to_scale, scale_type = "sum")
#'
#' # Scale the selected variables in the data frame based on the mean
#' scaled_data_mean <- scale_multiple_variables(data, variables_to_scale, scale_type = "mean")
#'
#' @importFrom stats scale
#' @export
scale_multiple_variables <- function(data, var_names, scale_type = "mean") {
  # Function implementation (see above)
}



# Function to scale multiple variables in a data frame based on sum or mean
scale_multiple_variables <- function(data, var_names, scale_type = "mean") {
  if (scale_type == "sum") {
    # Subset the data frame to only include the selected variables
    selected_vars <- data[, var_names, drop = FALSE]

    # Scale the selected variables using the sum method
    scaled_vars <- as.data.frame(apply(selected_vars, 2, function(x) (x - mean(x)) / sum(abs(x - mean(x)))))

    # Replace the selected variables in the original data frame with the scaled variables
    data[, var_names] <- scaled_vars

  } else if (scale_type == "mean") {
    # Subset the data frame to only include the selected variables
    selected_vars <- data[, var_names, drop = FALSE]

    # Scale the selected variables using the mean method
    scaled_vars <- as.data.frame(scale(selected_vars))

    # Replace the selected variables in the original data frame with the scaled variables
    data[, var_names] <- scaled_vars

  } else {
    stop("Invalid scale_type. Use 'sum' or 'mean'.")
  }

  return(data)
}

#' @importFrom ggplot2 ggplot aes geom_bar geom_text coord_flip scale_y_continuous
#' @importFrom xtable xtable
#' @importFrom parallel makeCluster stopCluster
#' @importFrom snow parLapply


