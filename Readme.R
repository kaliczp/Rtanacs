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
