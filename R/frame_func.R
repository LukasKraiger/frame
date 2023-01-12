#frame
#build and created by Lukas Kraiger, BSc

#Frequency Tables like SPSS
#F1 is Variable Name
#Var is the actual Variable


freqtable <- function(Var) {
  #'@import knitr
  #'@import kableExtra
  abs.Haeufigkeit <- table(Var)
  Prozent <- round(prop.table(abs.Haeufigkeit)*100, digits = 2)
  Skala <- unique(sort(Var))
  kum.Prozent <- round((cumsum(Prozent)/sum(Prozent))*100, digits = 2)
  kum.Haeufigkeit <- cumsum(abs.Haeufigkeit)
  tab <- cbind(Skala, abs.Haeufigkeit, kum.Haeufigkeit,Prozent,kum.Prozent)
  #rownames(tab) <- row
  knitr::kable(tab) #|>
  # kableExtra::add_footnote(foot, notation = "alphabet")
}



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


plot <- function(Data,df, x, y) {
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

ampelgrafik_proz <- function(Data, Label) {
  #'@import ggthemes
  #'@import ggplot2
  neu <- prozent5(Data)
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

prozent <- function(df) {
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


prozent5 <- function(df) {
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
