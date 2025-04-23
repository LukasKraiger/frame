#frame
#build and created by Lukas Kraiger, BSc

#Frequency Tables like SPSS
#F1 is Variable Name
#Var is the actual Variable

#' Frequency Table Generator
#'
#' This function generates a frequency table for a given variable.
#'
#' @import knitr
#'
#' @param Var The input variable for which the frequency table is to be generated.
#'
#' @return A table containing the absolute frequency, cumulative frequency,
#'   percentage, and cumulative percentage of the input variable.
#'
#' @examples
#' \dontrun{
#' # Generate a frequency table for a sample variable
#' sample_data <- c(1, 2, 1, 3, 2, 1, 2, 3, 3)
#' freqtable(sample_data)
#' }
#'
#' 
#' @importFrom stats prop.table
#' @importFrom base unique sort cumsum
#' @importFrom knitr kable
#' @importFrom utils round
#'
#' @export

  #'@import knitr
#' @importFrom vctrs 
#' @importFrom stats prop.table
#' @importFrom base unique sort cumsum
#' @importFrom knitr kable

#' @importFrom utils round
#'
#' @export
freqtable <- function(Var) {
  #'@import knitr
  abs.Haeufigkeit <- table(Var)
  Prozent <- round(prop.table(abs.Haeufigkeit)*100, digits = 2)
  Skala <- unique(sort(Var))
  kum.Prozent <- round((cumsum(Prozent)/sum(Prozent))*100, digits = 2)
  kum.Haeufigkeit <- cumsum(abs.Haeufigkeit)
  tab <- cbind(Skala, abs.Haeufigkeit, kum.Haeufigkeit,Prozent,kum.Prozent)
  #rownames(tab) <- row
  knitr::kable(tab)
  knitr::kable(tab)
}

#' Frequency Table Generator (new)
#'
#' This function generates a frequency table for a given variable.
#'
#' @import knitr
#'
#' @param Var The input variable for which the frequency table is to be generated.
#'
#' @return A table containing the absolute frequency, cumulative frequency,
#'   percentage, and cumulative percentage of the input variable.
#'
#' @examples
#' \dontrun{
#' # Generate a frequency table for a sample variable
#' sample_data <- c(1, 2, 1, 3, 2, 1, 2, 3, 3)
#' freqtable_new(sample_data)
#' }
#'
#' 
#' @importFrom stats prop.table
#' @importFrom base unique sort cumsum
#' @importFrom knitr kable
#' @importFrom utils round
#'
#' @export

  #'@import knitr
#' @importFrom vctrs 
#' @importFrom stats prop.table
#' @importFrom base unique sort cumsum
#' @importFrom knitr kable

#' @importFrom utils round
#'
#' @export

freqtable_new <- function(Var, VarNames) {
  #'@import knitr
  abs.Haeufigkeit <- table(Var)
  Prozent <- round(prop.table(abs.Haeufigkeit)*100, digits = 2)
  Skala <- unique(sort(Var))
  names(Skala) <- VarNames
  kum.Prozent <- round((cumsum(Prozent)/sum(Prozent))*100, digits = 2)
  kum.Haeufigkeit <- cumsum(abs.Haeufigkeit)
  tab <- cbind(Skala, abs.Haeufigkeit, kum.Haeufigkeit,Prozent,kum.Prozent)
  #rownames(tab) <- row
  knitr::kable(tab)
  knitr::kable(tab)
}



#' Descriptive Statistics Calculator
#'
#' This function calculates descriptive statistics for a given numeric data vector.
#'
#' @import knitr
#'
#' @param Data A numeric vector for which descriptive statistics are to be calculated.
#'
#' @return A table containing the mean, standard deviation, median, and the number of non-missing values (n).
#'
#' @examples
#' \dontrun{
#' # Calculate descriptive statistics for a sample numeric vector
#' sample_data <- c(1, 2, 3, 4, 5, NA)
#' deskreptiv_stat(sample_data)
#' }
#' @importFrom base as.numeric mean sd median length na.omit round
#' @importFrom knitr kable
#'
#' @export
deskreptiv_stat <- function(Data) {
  #'@import knitr
  Data_numeric <- as.numeric(Data)
  Mittelwert <- round(mean(Data_numeric, na.rm = TRUE), digits = 2)
  Kennwerte <- c("Mittelwert", "Standardabweichung", "Median", "n")
  Standardabweichung <- round(sd(Data_numeric, na.rm = TRUE), digits = 2)
  Median <- round(median(Data_numeric, na.rm = TRUE), digits = 2)
  n <- round(length(na.omit(Data_numeric)), digits = 2)
  Wert <- c(Mittelwert, Standardabweichung, Median, n)
  d <- cbind(Kennwerte,Wert)
  knitr::kable(head(d))
}

#' Descriptive Statistics Calculator (Transpose)
#'
#' This function calculates descriptive statistics for a given numeric data vector and returns
#' the result in a transposed format as a table.
#'
#' @import knitr
#'
#' @param Data A numeric vector for which descriptive statistics are to be calculated.
#'
#' @return A table containing the mean, standard deviation, median, and the number of non-missing values (n).
#'
#' @examples
#' \dontrun{
#' # Calculate descriptive statistics for a sample numeric vector
#' sample_data <- c(1, 2, 3, 4, 5, NA)
#' deskreptiv_stat_quer(sample_data)
#' }
#' @importFrom base as.numeric mean sd median length na.omit round
#' @importFrom knitr kable
#'
#' @export
deskreptiv_stat_quer <- function(Data) {
  #'@import knitr
  Data_numeric <- as.numeric(Data)
  Mittelwert <- round(mean(Data_numeric, na.rm = TRUE), digits = 2)
  Standardabweichung <- round(sd(Data_numeric, na.rm = TRUE), digits = 2)
  Median <- round(median(Data_numeric, na.rm = TRUE), digits = 2)
  n <- round(length(na.omit(Data_numeric)), digits = 2)
  d <- cbind(Mittelwert, Standardabweichung, Median, n)
  knitr::kable(head(d))
}


#plot
#data = Mentoren

#' Plot Percentage Bar Chart
#'
#' This function creates a percentage bar chart from a given data vector, where x represents the values
#' and y represents the variable being plotted. The function uses ggplot2 and ggthemes packages.
#'
#' @import ggthemes
#' @import ggplot2
#'
#' @param Data A numeric vector containing the data to be plotted.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#'
#' @return A percentage bar chart representing the distribution of the data.
#'
#' @examples
#' \dontrun{
#' # Create a percentage bar chart for sample data
#' sample_data <- c(30, 45, 15, 10)
#' plotproz(sample_data, "Category", "Percentage")
#' }
#' @importFrom base na.omit
#' @importFrom ggplot2 aes geom_bar stat_count labs xlim scale_x_continuous
#' @importFrom ggthemes theme_gdocs
#'
#' @export
plotproz <- function(Data, x, y) {
  #'@import ggthemes
  #'@import ggplot2
  xs <- na.omit(Data)
  ggplot2::ggplot(ggplot2::aes(xs)) +
    ggplot2::geom_bar(fill = ("#A71680"), #A2282E
                      alpha = 0.9) +
    ggplot2::stat_count(geom="text", ggplot2::aes(label=..count..), vjust=-0.5) +
    ggplot2::labs(y = y) +
    ggplot2::xlim(0, 100) +
    ggplot2::scale_x_continuous(name ="Prozent",
                                seq(0,100,10)) +
    ggthemes::theme_gdocs()
}

#' Plot Bar Chart
#'
#' This function creates a bar chart from a given data vector and a data frame, where x and y represent the
#' variables to be plotted on the x-axis and y-axis, respectively. The function uses ggplot2 and ggthemes packages.
#'
#' @import ggthemes
#' @import ggplot2
#'
#' @param Data A numeric vector containing the data to be used for plotting.
#' @param df A data frame containing the data to be used for plotting.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#'
#' @return A bar chart representing the distribution of the data.
#'
#' @examples
#' \dontrun{
#' # Create a bar chart for sample data
#' sample_data <- c(30, 45, 15, 10)
#' sample_df <- data.frame(Category = c("A", "B", "C", "D"), Values = sample_data)
#' plotbar(sample_data, sample_df, "Category", "Values")
#' }
#' @importFrom ggplot2 aes geom_bar stat_count labs theme_gdocs
#'
#' @export
plotbar <- function(Data,df, x, y) {
  #'@import ggthemes
  #'@import ggplot2
  xs <- Data
  ggplot2::ggplot(data= df, ggplot2::aes(xs)) +
    ggplot2::geom_bar(fill = ("#A71680"),
                      alpha = 0.9) +
    ggplot2::stat_count(geom="text", ggplot2::aes(label=..count..), vjust=-0.5) +
    ggplot2::labs(x = x, y = y) +
    ggthemes::theme_gdocs()
}

#' Plot Bar Chart with NA Count
#'
#' This function creates a bar chart from a given data vector, where x and y represent the
#' variables to be plotted on the x-axis and y-axis, respectively. The function omits NA values,
#' counts the occurrences of each value, and includes a caption showing the total number of non-NA
#' values and the number of NA values in the original data. The function uses ggplot2, ggthemes, dplyr,
#' and magrittr packages.
#'
#' @import ggthemes
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#'
#' @param Data A numeric vector containing the data to be used for plotting.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#'
#' @return A bar chart representing the distribution of the data without NA values, along with a caption
#'   showing the number of non-NA values and NA values in the original data.
#'
#' @examples
#' \dontrun{
#' # Create a bar chart with NA count for sample data
#' sample_data <- c(30, 45, NA, 10, NA, 20)
#' plot_nNA(sample_data, "Values", "Frequency")
#' }
#' @importFrom ggplot2 aes geom_bar stat_count labs theme_gdocs
#'
#' @export
plot_nNA <- function(Data, x, y) {
  #'@import ggthemes
  #'@import ggplot2
  #'@import dplyr
  #'@import magrittr
  df2 <- as.data.frame(na.omit(Data))
  colnames(df2) <- c("Spalte1")
  ggplot2::ggplot(data = df2, ggplot2::aes(Spalte1)) +
    ggplot2::geom_bar(fill = ("#A71680"),
                      alpha = 0.9) +
    ggplot2::stat_count(geom="text", ggplot2::aes(label=..count..), vjust=-0.5) +
    ggplot2::labs(x = x, y = y) +
    labs(caption = paste("N =", sum(!is.na(Data)),"NA =",sum(is.na(Data)))) +
    ggthemes::theme_gdocs()
}

#' Traffic Light Chart with Percentages
#'
#' This function creates a traffic light chart with percentages from a given data vector and label.
#' The function first calculates percentages using the 'prozent5' function (assuming it's defined elsewhere),
#' then creates the chart using ggplot2 and ggthemes packages.
#'
#' @import ggthemes
#' @import ggplot2
#'
#' @param Data A numeric vector containing the data to be used for plotting.
#' @param Label The label for the x-axis.
#'
#' @return A traffic light chart with percentages showing the distribution of the data.
#'
#' @examples
#' \dontrun{
#' # Create a traffic light chart with percentages for sample data
#' sample_data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5)
#' ampelgrafik_proz(sample_data, "Category")
#' }
#' @importFrom ggplot2 aes geom_bar geom_text coord_flip scale_y_continuous labs
#'
#' @export
ampelgrafik_proz <- function(Data, Label) {
  # Wende die vereinfachte prozent5-Funktion an
  neu <- prozent5(Data)
  
  # Erstelle eine Tabelle mit Häufigkeiten
  data <- as.data.frame(table(neu))
  hundret <- sum(data$Freq)
  
  # Berechne die Prozentwerte
  data$Prozentlabel <- round(data$Freq / hundret, digits = 2)
  
  # Farben für die Balken
  bar_colors <- c("#C00000", "#FBBE02", "#FFFF00", "#92D050", "#0B8E00")
  
  # Schriftfarben für die Textlabels
  text_colors <- c("white", "black", "black", "black", "white")  # Weiß für Rot und Dunkelgrün
  
  # Erstelle das Diagramm
  ggplot(data, aes(fill = forcats::fct_rev(neu), y = Freq, x = Label)) +
    geom_bar(position = "fill", width = 0.2, stat = "identity", fill = bar_colors) +  # Balkenbreite reduziert
    geom_text(
      size = 4, 
      position = position_fill(vjust = 0.5), 
      aes(label = scales::percent(Prozentlabel), color = forcats::fct_rev(neu))
    ) +
    scale_color_manual(values = text_colors) +  # Schriftfarben anpassen
    ggplot2::labs(x = NULL, y = NULL) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none")  # Legende ausblenden, falls nicht benötigt
}

#' Likert-style Traffic Light Chart with Percentages
#'
#' This function creates a likert-style traffic light chart with percentages from a given data vector and label.
#' The function first converts the data to numeric, calculates percentages, and then creates the chart using ggplot2
#' and ggthemes packages.
#'
#' @import ggthemes
#' @import ggplot2
#'
#' @param Data A numeric vector containing the data to be used for plotting.
#' @param Label The label for the x-axis.
#'
#' @return A likert-style traffic light chart with percentages showing the distribution of the data.
#'
#' @examples
#' \dontrun{
#' # Create a likert-style traffic light chart with percentages for sample data
#' sample_data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5)
#' ampelgrafik_likert(sample_data, "Category")
#' }
#' @importFrom ggplot2 aes geom_bar geom_text coord_flip scale_y_continuous labs
#'
#' @export
ampelgrafik_likert <- function(Data, Label) {
  neu <- as.numeric(Data)
  data<- as.data.frame(table(neu))
  hundret <- sum(data$Freq)
  data$Prozentlabel <- round(data$Freq/hundret, digits = 2)
  specie <- Label
  # Stacked + percent
  ggplot(data, aes(fill=forcats::fct_rev(neu), y=Freq, x=specie)) +
    geom_bar(position="fill", width = 0.5, stat="identity", fill = (c("#C00000", "#FBBE02", "#FFFF00", "#92D050",
                                                                               "#0B8E00")))+
                                                                                 geom_text(size = 4, position = position_fill(vjust = 0.5), aes(label=scales::percent(Prozentlabel))) +
    ggplot2::labs(x = NULL, y = NULL ) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent)
}

#' Categorize Values into Percentage Ranges
#'
#' This function takes a numeric vector and categorizes its values into percentage ranges.
#'
#' @param df A numeric vector or data frame containing the values to be categorized.
#'
#' @return A data frame with an additional column "categories" that contains the corresponding percentage range for each value.
#'
#' @details The function uses the `cut()` function to categorize the values into ten percentage ranges: 0-10, 11-20, 21-30, 31-40, 41-50, 51-60, 61-70, 71-80, 81-90, and 91-100.
#'
#' @examples
#' \dontrun{
#' df <- c(10, 35, 50, 75, 90)
#' categorized_df <- prozent(df)
#' }
#' @importFrom stats cut
#'
#' @export
prozent <- function(df) {
  breaks <- c(-Inf, seq(0, 100, 10), Inf)
  labels <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")

  df$categories <- cut(df, breaks = breaks, labels = labels, right = FALSE)
  return(df)
}


.prozent_legacy <- function(df) {
  df[df == 0] <- "0-10"
  df[df == 1] <- "0-10"
  df[df == 2] <- "0-10"
  df[df == 3] <- "0-10"
  df[df == 4] <- "0-10"
  df[df == 5] <- "0-10"
  df[df == 6] <- "0-10"
  df[df == 7] <- "0-10"
  df[df == 8] <- "0-10"
  df[df == 9] <- "0-10"
  df[df == 10] <- "0-10"
  df[df == 11] <- "11-20"
  df[df == 12] <- "11-20"
  df[df == 13] <- "11-20"
  df[df == 14] <- "11-20"
  df[df == 15] <- "11-20"
  df[df == 16] <- "11-20"
  df[df == 17] <- "11-20"
  df[df == 18] <- "11-20"
  df[df == 19] <- "11-20"
  df[df == 20] <- "11-20"
  df[df == 21] <- "21-30"
  df[df == 22] <- "21-30"
  df[df == 23] <- "21-30"
  df[df == 24] <- "21-30"
  df[df == 25] <- "21-30"
  df[df == 26] <- "21-30"
  df[df == 27] <- "21-30"
  df[df == 28] <- "21-30"
  df[df == 29] <- "21-30"
  df[df == 30] <- "21-30"
  df[df == 31] <- "31-40"
  df[df == 32] <- "31-40"
  df[df == 33] <- "31-40"
  df[df == 34] <- "31-40"
  df[df == 35] <- "31-40"
  df[df == 36] <- "31-40"
  df[df == 37] <- "31-40"
  df[df == 38] <- "31-40"
  df[df == 39] <- "31-40"
  df[df == 40] <- "31-40"
  df[df == 41] <- "41-50"
  df[df == 42] <- "41-50"
  df[df == 43] <- "41-50"
  df[df == 44] <- "41-50"
  df[df == 45] <- "41-50"
  df[df == 46] <- "41-50"
  df[df == 47] <- "41-50"
  df[df == 48] <- "41-50"
  df[df == 49] <- "41-50"
  df[df == 50] <- "41-50"
  df[df == 51] <- "51-60"
  df[df == 52] <- "51-60"
  df[df == 53] <- "51-60"
  df[df == 54] <- "51-60"
  df[df == 55] <- "51-60"
  df[df == 56] <- "51-60"
  df[df == 57] <- "51-60"
  df[df == 58] <- "51-60"
  df[df == 59] <- "51-60"
  df[df == 60] <- "51-60"
  df[df == 61] <- "61-70"
  df[df == 62] <- "61-70"
  df[df == 63] <- "61-70"
  df[df == 64] <- "61-70"
  df[df == 65] <- "61-70"
  df[df == 66] <- "61-70"
  df[df == 67] <- "61-70"
  df[df == 68] <- "61-70"
  df[df == 69] <- "61-70"
  df[df == 70] <- "61-70"
  df[df == 71] <- "71-80"
  df[df == 72] <- "71-80"
  df[df == 73] <- "71-80"
  df[df == 74] <- "71-80"
  df[df == 75] <- "71-80"
  df[df == 76] <- "71-80"
  df[df == 77] <- "71-80"
  df[df == 78] <- "71-80"
  df[df == 79] <- "71-80"
  df[df == 80] <- "71-80"
  df[df == 81] <- "81-90"
  df[df == 82] <- "81-90"
  df[df == 83] <- "81-90"
  df[df == 84] <- "81-90"
  df[df == 85] <- "81-90"
  df[df == 86] <- "81-90"
  df[df == 87] <- "81-90"
  df[df == 88] <- "81-90"
  df[df == 89] <- "81-90"
  df[df == 90] <- "81-90"
  df[df == 91] <- "91-100"
  df[df == 92] <- "91-100"
  df[df == 93] <- "91-100"
  df[df == 94] <- "91-100"
  df[df == 95] <- "91-100"
  df[df == 96] <- "91-100"
  df[df == 97] <- "91-100"
  df[df == 98] <- "91-100"
  df[df == 99] <- "91-100"
  df[df == 100] <- "91-100"
  return(df)

}

#' Categorize Values into Percentage Ranges
#'
#' This function takes a numeric vector and categorizes its values into percentage ranges.
#'
#' @param df A numeric vector or data frame containing the values to be categorized.
#'
#' @return A data frame with an additional column "categories" that contains the corresponding percentage range for each value.
#'
#' @details The function uses the `cut()` function to categorize the values into five percentage ranges: 0-20, 21-40, 41-60, 61-80, and 81-100.
#'
#' @examples
#' \dontrun{
#' df <- c(10, 35, 50, 75, 90)
#' categorized_df <- prozent5(df)
#' }
#' @importFrom stats cut
#'
#' @export
prozent5 <- function(data) {
  # Definiere die Intervalle (breaks) und die zugehörigen Labels
  breaks <- c(0, 20, 40, 60, 80, 100)  # 5 Intervalle
  labels <- c("0-20", "21-40", "41-60", "61-80", "81-100")  # 5 Labels
  
  # Teile die Daten in Kategorien ein
  categories <- cut(data, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)
  
  # Gib die Kategorien zurück
  return(categories)
}




.prozent5_legacy <- function(df) {
  df[df == 0] <- "0-20"
  df[df == 1] <- "0-20"
  df[df == 2] <- "0-20"
  df[df == 3] <- "0-20"
  df[df == 4] <- "0-20"
  df[df == 5] <- "0-20"
  df[df == 6] <- "0-20"
  df[df == 7] <- "0-20"
  df[df == 8] <- "0-20"
  df[df == 9] <- "0-20"
  df[df == 10] <- "0-20"
  df[df == 11] <- "0-20"
  df[df == 12] <- "0-20"
  df[df == 13] <- "0-20"
  df[df == 14] <- "0-20"
  df[df == 15] <- "0-20"
  df[df == 16] <- "0-20"
  df[df == 17] <- "0-20"
  df[df == 18] <- "0-20"
  df[df == 19] <- "0-20"
  df[df == 20] <- "0-20"
  df[df == 21] <- "21-40"
  df[df == 22] <- "21-40"
  df[df == 23] <- "21-40"
  df[df == 24] <- "21-40"
  df[df == 25] <- "21-40"
  df[df == 26] <- "21-40"
  df[df == 27] <- "21-40"
  df[df == 28] <- "21-40"
  df[df == 29] <- "21-40"
  df[df == 30] <- "21-40"
  df[df == 31] <- "21-40"
  df[df == 32] <- "21-40"
  df[df == 33] <- "21-40"
  df[df == 34] <- "21-40"
  df[df == 35] <- "21-40"
  df[df == 36] <- "21-40"
  df[df == 37] <- "21-40"
  df[df == 38] <- "21-40"
  df[df == 39] <- "21-40"
  df[df == 40] <- "21-40"
  df[df == 41] <- "41-60"
  df[df == 42] <- "41-60"
  df[df == 43] <- "41-60"
  df[df == 44] <- "41-60"
  df[df == 45] <- "41-60"
  df[df == 46] <- "41-60"
  df[df == 47] <- "41-60"
  df[df == 48] <- "41-60"
  df[df == 49] <- "41-60"
  df[df == 50] <- "41-60"
  df[df == 51] <- "41-60"
  df[df == 52] <- "41-60"
  df[df == 53] <- "41-60"
  df[df == 54] <- "41-60"
  df[df == 55] <- "41-60"
  df[df == 56] <- "41-60"
  df[df == 57] <- "41-60"
  df[df == 58] <- "41-60"
  df[df == 59] <- "41-60"
  df[df == 60] <- "41-60"
  df[df == 61] <- "61-80"
  df[df == 62] <- "61-80"
  df[df == 63] <- "61-80"
  df[df == 64] <- "61-80"
  df[df == 65] <- "61-80"
  df[df == 66] <- "61-80"
  df[df == 67] <- "61-80"
  df[df == 68] <- "61-80"
  df[df == 69] <- "61-80"
  df[df == 70] <- "61-80"
  df[df == 71] <- "61-80"
  df[df == 72] <- "61-80"
  df[df == 73] <- "61-80"
  df[df == 74] <- "61-80"
  df[df == 75] <- "61-80"
  df[df == 76] <- "61-80"
  df[df == 77] <- "61-80"
  df[df == 78] <- "61-80"
  df[df == 79] <- "61-80"
  df[df == 80] <- "61-80"
  df[df == 81] <- "81-100"
  df[df == 82] <- "81-100"
  df[df == 83] <- "81-100"
  df[df == 84] <- "81-100"
  df[df == 85] <- "81-100"
  df[df == 86] <- "81-100"
  df[df == 87] <- "81-100"
  df[df == 88] <- "81-100"
  df[df == 89] <- "81-100"
  df[df == 90] <- "81-100"
  df[df == 91] <- "81-100"
  df[df == 92] <- "81-100"
  df[df == 93] <- "81-100"
  df[df == 94] <- "81-100"
  df[df == 95] <- "81-100"
  df[df == 96] <- "81-100"
  df[df == 97] <- "81-100"
  df[df == 98] <- "81-100"
  df[df == 99] <- "81-100"
  df[df == 100] <- "81-100"
  return(df)
}

#' Render R Markdown Files in Parallel
#'
#' This function renders all R Markdown files in the specified \code{PATH} using
#' parallel processing with the given number of \code{cores}.
#'
#' @param cores Number of CPU cores to be used for parallel rendering.
#' @param PATH The path to the directory containing the R Markdown files to be rendered.
#'
#' @details This function uses parallel processing to render R Markdown files in
#' the specified \code{PATH}. Each R Markdown file will be rendered using a
#' separate core, which can significantly speed up the rendering process if
#' multiple cores are available.
#'
#' @examples
#' # Render R Markdown files in the "documents" directory using 4 cores
#' # Renderengine(4, "documents")
#'
#' @import parallel
#' @import rmarkdown
#'
#' @export
#'
#' @seealso \code{\link{makeCluster}}, \code{\link{parLapply}}
#'
#' @keywords R Markdown parallel rendering
Renderengine <- function(cores, PATH) {
  files <- list.files(pattern = "[.]Rmd$")

  # Create a cluster using the specified number of cores
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))  # Make sure to stop the cluster even if an error occurs

  # Function to render the R Markdown file
  render_file <- function(file) {
    tryCatch(
      rmarkdown::render(file, output_dir = PATH),
      error = function(e) cat("Error rendering", file, "\n", conditionMessage(e), "\n")
    )
  }

  # Use parallel processing to render the files
  parLapply(cl, files, render_file)
}
