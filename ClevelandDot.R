corn.mat <- as.matrix(corn[,-1])
row.names(corn.mat) <- corn[,1]
corn.mat <- corn.mat[nrow(corn.mat):1,]
library(lattice)
dotplot(corn.mat,
        groups = FALSE,
        scales = list(y = list(cex=0.7)))

pdf(height=23/2.54, width = 18/2.54)
