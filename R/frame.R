#CopyCat

Variable_Copy <- function(df) {
  var <- paste0("df$",df)
  line <- c(paste0(" `r ",var, "%>% attr('label')`"),cat("\n"), paste0("freqtable(", var, ")"),
                    paste0("#deskreptiv_stat(", var, ")\n", paste0("#deskreptiv_stat_quer(", var, ")\n"),
                    paste0("#plot(df,", var, "x,y)\n"),paste0("#plotproz(df,", var, "x,y)\n"),
                    paste0("#plot_nNA(df,", var, "x,y)\n")), paste0("#ampelgrafik(df,", var, "'Label'')\n"))
  write(line,file="Report.R",append=TRUE)
}


frame <- function() {
  file.remove("Report.R", "Report.Rmd", "Report.md", "Report.html")
  df_place<- file.choose()
  df<- sjlabelled::read_spss(df_place)
  data<- sapply(df, class)
  data <- replace(data, data=="factor", "numeric")
   df2 <- data.frame(data)
   numeric <- subset(df2, data == "numeric")
    Variablen <- row.names(numeric)
    Source_Tab <- (paste0("
      ---
      title: 'Titel'
      author: 'Dr. Gregor Joestl & Lukas Kraiger, BSc'
      date: '`r Sys.Date()`'
      output:
      #word_document:
      #toc: yes
      pdf_document:
      toc: yes
      ---


      knitr::opts_chunk$set(
	    echo = FALSE
      )
      library(frame)
      library(haven)
      library(magrittr)
      library(ggplot2)
      library(dplyr)
      library(sjlabelled)
      df<- sjlabelled::read_spss('",df_place, "')

      "))

    write(Source_Tab, file = "Report.R", append = TRUE)

    sapply(Variablen, frame::Variable_Copy)


    knitr::spin('Report.R', precious = TRUE, doc = '#')
}


