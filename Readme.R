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

    out.loll <- ggplot(data = x, aes(x=.data[[variable]], y=Country)) +
        geom_segment( aes(yend=Country, xend=0), colour= colourbysign) +
        geom_vline(xintercept=0, colour = "darkgrey") +
        geom_point(size=3, colour = colourbysign) +
        theme_classic() +
        labs(y = NULL) +
        guides(y = "none") +
        theme(
              panel.border = element_blank())
    pos.lab <- subset(x, get(variable) >= 0)
    neg.lab <- subset(x, get(variable) < 0)
    if(nrow(pos.lab) > 0) {
        out.loll <- out.loll + geom_text(aes(x = -1, label = Country, hjust = "right"),
                                         data = pos.lab,
                                         colour = "darkgreen")
                if(nrow(neg.lab) == 0) {
            out.loll <- out.loll +
                theme(plot.margin = margin(0, 0, 0, 1.5, 'cm')) +
                coord_cartesian(clip = "off")
            }
    }
    if(nrow(neg.lab) > 0){
        out.loll <- out.loll + geom_text(aes(x = 1, label = Country, hjust = "left"),
                                         data = neg.lab,
                                         colour = "darkred")
        if(nrow(pos.lab) == 0) {
            out.loll <- out.loll +
                theme(plot.margin = margin(0, 1.5, 0, 0, 'cm')) +
                coord_cartesian(clip = "off")
            }
    }
    out.loll
}

wheat.loll <- SimpleLolly(corn, variable = "Wheat")
maize.loll <- SimpleLolly(corn, variable = "Maize")

pdf()
wheat.loll
maize.loll
dev.off()
