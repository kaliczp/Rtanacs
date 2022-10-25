corn.col$CCCode <- paste(corn.col$Country, corn.col$Corn, sep = "__")
corn.col$CCCode <- factor(corn.col$CCCode, levels = corn.col[order(corn.col$Perc), "CCCode"])


ReorderedLolly <- function(x, variable, na.rm = FALSE) {
    if(na.rm)
        x <- x[!is.na(x[, variable]),]
    colourbysign <- ifelse(x[, variable] > 0, "darkgreen", "darkred")
    fontcolour <- "white"
    cornlabel <- round(x[, variable])
    out <- ggplot(data = x, aes(x=.data[[variable]], y=CCCode)) +
        geom_segment( aes(yend=CCCode, xend=0), colour= colourbysign) +
        geom_vline(xintercept=0, colour = "darkgrey") +
        geom_point(size=7, colour = colourbysign) +
        facet_wrap(~Corn, scales = "free_y") +
        theme_classic() +
        labs(x = NULL, y = NULL) +
        coord_cartesian(clip = "off") +
        scale_y_discrete(labels = function(x) gsub("__.+$", "", x)) +
        geom_text(aes(label = cornlabel), colour = fontcolour, size = 3)
    out
}

ReorderedLolly(corn.col, "Perc", na.rm = TRUE)

pdf(paste0("GgdotchartBuborékban_rank6.pdf"), height = 27 / 2.54, width = 18 / 2.54)
ReorderedLolly(corn.col, "Perc")
dev.off()
