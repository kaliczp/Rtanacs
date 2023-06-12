library(readxl)
eur.raw <- read_excel("Reviewre/fig_3.xlsx")

eur.df <- data.frame(Country = eur.raw[,1, drop = TRUE],
                     Income = eur.raw[,2, drop = TRUE],
                     Corn = "EUR"
                     )
eur.df$CCCode <- paste0(eur.df$Country, "__EUR")
eur.df$CCCode <- factor(eur.df$CCCode, levels = eur.df[nrow(eur.df):1, "CCCode"])

ReorderedLolly(eur.df, "Income", yaxis = FALSE)
