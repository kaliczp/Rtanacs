library(readxl)
eur.raw <- read_excel("Reviewre/fig_3.xlsx")

eur.df <- data.frame(Country = eur.raw[,1, drop = TRUE],
                     Income = eur.raw[,2, drop = TRUE],
                     Corn = "EUR"
                     )
