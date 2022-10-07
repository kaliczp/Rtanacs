library(readxl)
ggdotchart <- as.data.frame(read_excel("ggdotchart.xlsx"))
## lollipop_chart
library(ggplot2)
library(ggcharts)

diverging_lollipop_chart(
    data = ggdotchart, x = Country, y = Wheat,                              
    lollipop_colors = c("darkgreen", "darkred"),
    line_size = 0.75,
    point_size = 3,
    text_color = c("darkgreen", "darkred"),
    text_size = 10)

ggdotchart$Country <- factor(ggdotchart$Country, levels = ggdotchart$Country)

colourbysign <- ifelse(ggdotchart$Wheat > 0, "darkgreen", "darkred")

wheat.loll <- ggplot(data = ggdotchart, aes(x=Country, y=Wheat)) +
    geom_segment( aes(xend=Country, yend=0), colour= colourbysign) +
    geom_point( size=3, colour = colourbysign) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.y = element_text(colour = colourbysign))


maize <- as.data.frame(read_excel("ggdotchart.xlsx", sheet = 2))

diverging_lollipop_chart(
    data = maize, x = Country, y = Maize,
    lollipop_colors = c("darkgreen", "darkred"),
    line_size = 0.75,
    point_size = 3,
    text_color = c("darkgreen", "darkred"),
    text_size = 10)

maize$Country <- factor(maize$Country, levels = maize$Country)

maize.loll <- ggplot(data = maize, aes(x=Country, y=Maize)) +
    geom_segment( aes(xend=Country, yend=0), color="darkred") +
    geom_point( size=3, color="darkred") +
    coord_flip() +
    theme_bw() +
    theme(axis.text.y = element_text(colour = "darkred"))

pdf()
wheat.loll
maize.loll
dev.off()
