library(readxl)
corn <- as.data.frame(read_excel("ggdotchartall.xlsx"))
corn <- corn[,-6]
corn$Country <- factor(corn$Country,
                       levels = corn[order(as.vector(corn$Country), decreasing = TRUE),
                                     "Country"]
                       )

tti <- 2
corn.col <- corn[, c(1,tti)]
corn.col$Corn <- colnames(corn)[tti]
colnames(corn.col) <- c("Country", "Perc", "Corn")
for(tti in 3:ncol(corn)) {
    ttcorn <- corn[, c(1, tti)]
    ttcorn$Corn <- colnames(corn)[tti]
    colnames(ttcorn) <- c("Country", "Perc", "Corn")
    corn.col <- rbind(corn.col, ttcorn)
}

library(ggplot2)

SimpleLolly <- function(x, variable){
    ## Colour by sing
    colourbysign <- ifelse(x[, variable] > 0, "darkgreen", "darkred")
    out.loll <- ggplot(data = x, aes(x=.data[[variable]], y=Country)) +
        geom_segment( aes(yend=Country, xend=0), colour= colourbysign) +
        geom_vline(xintercept=0, colour = "darkgrey") +
        geom_point(size=8.5, colour = colourbysign) +
        geom_text(aes(label = round(.data[[variable]])), colour = "white") +
        facet_wrap(~Corn) +
        theme_classic() +
        labs(x = NULL, y = NULL) +
        coord_cartesian(clip = "off")
    out.loll
}

SimpleLolly(corn.col, "Perc")

pdf(paste0("GgdotchartBuborékban.pdf"), height = 27 / 2.54, width = 18 / 2.54)
SimpleLolly(corn.col, "Perc")
dev.off()
