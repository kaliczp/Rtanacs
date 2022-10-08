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
        geom_hline(yintercept=0, colour = "darkgrey") +
        geom_point( size=3, colour = colourbysign) +
        coord_flip() +
        theme_classic() +
        labs(x = NULL) +
        guides(y = "none") +
        theme(
              panel.border = element_blank())
    pos.lab <- subset(x, get(variable) >= 0)
    if(nrow(pos.lab) > 0) {
        out.loll <- out.loll + geom_text(aes(y = -1, label = Country, hjust = "right"),
                                         data = pos.lab,
                                         colour = "darkgreen")
    }
    neg.lab <- subset(x, get(variable) < 0)
    if(nrow(neg.lab) > 0){
        out.loll <- out.loll + geom_text(aes(y = 1, label = Country, hjust = "left"),
                                         data = neg.lab,
                                         colour = "darkred")
    }
    out.loll
}

wheat.loll <- SimpleLolly(corn, variable = "Wheat")
maize.loll <- SimpleLolly(corn, variable = "Maize")

pdf()
wheat.loll
maize.loll
dev.off()
