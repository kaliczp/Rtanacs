library(readxl)
corn <- as.data.frame(read_excel("ggdotchartall.xlsx"))
library(ggplot2)

SimpleLolly <- function(x, variable){
    ## Delete NA rows in varialble
    x <- x[!is.na(x[, variable]),]
    ## Order variables
    x <- x[order(x[, variable]), ]
    ## Labels in the right order
    x$Country <- factor(x$Country, levels = x$Country)
    ## Colour by sing
    colourbysign <- ifelse(x[, variable] > 0, "darkgreen", "darkred")

    out.loll <- ggplot(data = x, aes(x=Country, y=.data[[variable]])) +
        geom_segment( aes(xend=Country, yend=0), colour= colourbysign) +
        geom_point( size=3, colour = colourbysign) +
        coord_flip() +
        theme_classic() +
        theme(axis.text.y = element_text(colour = colourbysign),
              panel.border = element_blank())

    out.loll
}

wheat.loll <- SimpleLolly(corn, variable = "Wheat")
maize.loll <- SimpleLolly(corn, variable = "Maize")

pdf()
wheat.loll
maize.loll
dev.off()
