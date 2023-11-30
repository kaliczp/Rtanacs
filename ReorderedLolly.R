corn.col$CCCode <- paste(corn.col$Country, corn.col$Corn, sep = "__")
corn.col$CCCode <- factor(corn.col$CCCode, levels = corn.col[order(corn.col$Perc), "CCCode"])


ReorderedLolly <- function(x, variable, na.rm = FALSE, yaxis = TRUE) {
    if(na.rm)
        x <- x[!is.na(x[, variable]),]
    colourbysign <- ifelse(x[, variable] > 0, "darkgreen", "darkred")
    fontcolour <- "white"
    cornlabel <- round(x[, variable])
    out <- ggplot(data = x, aes(x=.data[[variable]], y=CCCode)) +
        geom_segment( aes(yend=CCCode, xend=0), colour= colourbysign) +
        geom_vline(xintercept=0, colour = "darkgrey") +
        geom_point(size=6.5, colour = colourbysign) +
        facet_wrap(~Corn, scales = "free_y") +
        theme_classic() +
        labs(x = NULL, y = NULL) +
        coord_cartesian(clip = "off") +
        geom_text(aes(label = cornlabel), colour = fontcolour, size = 2.9)
    if(yaxis) {
        out <- out + scale_y_discrete(labels = function(x) gsub("__.+$", "", x))
    } else {
        pos.lab <- subset(x, get(variable) >= 0)
        pos.lab$xpos <- ifelse(pos.lab[, variable] < 3.5, -4, -1)
        out <- out + geom_text(aes(x = xpos, label = Country, hjust = "right"),
                               data = pos.lab,
                               colour = "darkgreen")
        neg.lab <- subset(x, get(variable) < 0)
        neg.lab$xpos <- ifelse(neg.lab[, variable] > -3.5, 4, 1)
        out <- out + geom_text(aes(x = xpos, label = Country, hjust = "left"),
                               data = neg.lab,
                               colour = "darkred") +
            guides(y = "none")
    }
    out
}

ReorderedLolly(corn.col, "Perc", na.rm = TRUE, yaxis = FALSE)

pdf(paste0("GgdotchartBuborékban_rank6.pdf"), height = 22.5 / 2.54, width = 18 / 2.54, pointsize = 5)
ReorderedLolly(corn.col, "Perc", yaxis = FALSE)
dev.off()
