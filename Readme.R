library(readxl)
corn <- as.data.frame(read_excel("ggdotchartall.xlsx"))
corn <- corn[,-6]
corn$Country <- factor(corn$Country,
                       levels = corn[order(as.vector(corn$Country), decreasing = TRUE),
                                     "Country"]
                       )

library(ggplot2)

SimpleLolly <- function(x, variable){
    ## Colour by sing
    colourbysign <- ifelse(x[, variable] > 0, "darkgreen", "darkred")
    out.loll <- ggplot(data = x, aes(x=.data[[variable]], y=Country)) +
        geom_segment( aes(yend=Country, xend=0), colour= colourbysign) +
        geom_vline(xintercept=0, colour = "darkgrey") +
        geom_point(size=8.5, colour = colourbysign) +
        geom_text(aes(label = round(.data[[variable]])), colour = "white") +
        theme_classic() +
        labs(y = NULL)
    out.loll
}

Wheat.loll <- SimpleLolly(corn, variable = "Wheat")
Maize.loll <- SimpleLolly(corn, variable = "Maize")
Barley.loll <- SimpleLolly(corn, variable = "Barley")
Rye.loll <- SimpleLolly(corn, variable = "Rye")
Rapeseed.loll <- SimpleLolly(corn, variable = "Rapeseed")
Sunflower.loll <- SimpleLolly(corn, variable = "Sunflower")

tti <- 2 # Replace with column number
pdf(paste0("GgdotchartBuborékban", tti, ".pdf"), width = 10 / 2.54)
get(paste0(names(corn)[tti], ".loll"))
dev.off()
