library(readxl)
ttsheet <- 1
corn <- as.data.frame(read_excel("ggdotchartall.xlsx", ttsheet))
corn[,1] <- factor(corn[,1], levels = corn[order(corn[,1], decreasing = TRUE), 1])

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
corn.col$Corn <- factor(corn.col$Corn,
                       levels = colnames(corn[, -1]),
                       )


library(ggplot2)

SimpleLolly <- function(x, variable, bw = FALSE, lolly = TRUE){
    if(bw) {
        ## Grey by sing
        colourbysign <- ifelse(x[, variable] > 0, "grey50", "grey70")
        fontcolour <- "black"
    } else {
        ## Colour by sing
        colourbysign <- ifelse(x[, variable] > 0, "darkgreen", "darkred")
        fontcolour <- "white"
    }
    cornlabel <- round(x[, variable])
    out.nololly <- ggplot(data = x, aes(x=.data[[variable]], y=Country)) +
        geom_segment( aes(yend=Country, xend=0), colour= colourbysign) +
        geom_vline(xintercept=0, colour = "darkgrey")
    if(lolly) {
        out.notext <- out.nololly +
            geom_point(size=7, colour = colourbysign)
    } else {
        out.notext <- out.nololly
    }
    if(bw & !lolly) {
        ## Prepare font placement
        nudge_text <- ifelse(x[, variable] > 0, 6, -6)
        nudge_div <- ifelse(abs(x[, variable]) < 9.5, 2, 1)
        nudge_text <- nudge_text / nudge_div
        out.nofacet <- out.notext +
            geom_text(aes(label = cornlabel), colour = fontcolour, size = 3, nudge_x = nudge_text)
    } else {
        out.nofacet <- out.notext +
            geom_text(aes(label = cornlabel), colour = fontcolour, size = 3)
    }
    out.loll <- out.nofacet +
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
