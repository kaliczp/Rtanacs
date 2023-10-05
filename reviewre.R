library(readxl)
ttsheet  <- 2 # 1 English, 2 Hungarian
eur.raw <- read_excel("Reviewre/fig_3.xlsx", ttsheet)

eur.df <- data.frame(Country = eur.raw[,1, drop = TRUE],
                     Income = eur.raw[,2, drop = TRUE],
                     Corn = "millió EUR"
                     )
eur.df$CCCode <- paste0(eur.df$Country, "__EUR")
eur.df$CCCode <- factor(eur.df$CCCode, levels = eur.df[nrow(eur.df):1, "CCCode"])

pdf("GgdotchartEUR.pdf", height = 11.25 / 2.54, width = 6 / 2.54, pointsize = 5)
ReorderedLolly(eur.df, "Income", yaxis = FALSE, xlim = c(-3350, 1350),
               shape = "rectangle", col = c("#5d6683", "#af7f6b"),
               lab.dist.default = 90, close.lab.threshold = 250, colse.lab.factor = 1.2)
dev.off()

## Balck-and-white
pdf("GgdotchartEURff.pdf", height = 11.25 / 2.54, width = 6 / 2.54, pointsize = 5)
ReorderedLolly(eur.df, "Income", yaxis = FALSE, xlim = c(-3360, 1440),
               shape = "rectangle", col = c("darkgray", "black"),
               lab.dist.default = 90, close.lab.threshold = 250, colse.lab.factor = 1.2) +
    theme( strip.background = element_blank(),
          strip.text.x = element_blank()) +
    annotate("text", label = "millió EUR", size = 4, x = -3360, y = 14, angle = 90)
dev.off()
