library(readxl)
eur.raw <- read_excel("Reviewre/fig_3.xlsx")

eur.df <- data.frame(Country = eur.raw[,1, drop = TRUE],
                     Income = eur.raw[,2, drop = TRUE],
                     Corn = "EUR"
                     )
eur.df$CCCode <- paste0(eur.df$Country, "__EUR")
eur.df$CCCode <- factor(eur.df$CCCode, levels = eur.df[nrow(eur.df):1, "CCCode"])

pdf("GgdotchartEUR.pdf", height = 11.25 / 2.54, width = 6 / 2.54, pointsize = 5)
ReorderedLolly(eur.df, "Income", yaxis = FALSE, xlim = c(-3350, 1350),
               shape = "rectangle", col = c("#5d6683", "#af7f6b"),
               lab.dist.default = 90, close.lab.threshold = 250, colse.lab.factor = 1.2)
dev.off()
