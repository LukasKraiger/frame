% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frame_func.R
\name{deskreptiv_stat_quer}
\alias{deskreptiv_stat_quer}
\title{Descriptive Statistics Calculator (Transpose)}
\usage{
deskreptiv_stat_quer(Data)
}
\arguments{
\item{Data}{A numeric vector for which descriptive statistics are to be calculated.}
}
\value{
A table containing the mean, standard deviation, median, and the number of non-missing values (n).
}
\description{
This function calculates descriptive statistics for a given numeric data vector and returns
the result in a transposed format as a table.
}
\examples{
\dontrun{
# Calculate descriptive statistics for a sample numeric vector
sample_data <- c(1, 2, 3, 4, 5, NA)
deskreptiv_stat_quer(sample_data)
}
}
