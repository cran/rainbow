fbag <- function (data, factor = 2.57, xlim = NULL, ylim = range(data$y, na.rm = TRUE), xlab, ylab, ...) 
{ 
    y <- t(data$y)
    x <- data$x
    rob <- PCAproj(y, k = 2, center = median)$score    
    pcbag <- compute.bagplot(rob[, 1], rob[, 2], factor = factor)
    if(pcbag$is.one.dim == TRUE)
    {
       stop("Bivariate principal component scores lie in one direction.")  
    }
    else{
        outlier <- as.numeric(rownames(pcbag$pxy.outlier))
        inside <- as.numeric(rownames(pcbag$pxy.bag))
        insidecurve <- y[inside, ]
        maximum1 <- apply(insidecurve, 2, max)
        minimum1 <- apply(insidecurve, 2, min)
        out <- as.numeric(rownames(pcbag$pxy.outer))
        outcurve <- y[out, ]
        maximum2 <- apply(outcurve, 2, max)
        minimum2 <- apply(outcurve, 2, min)
        p = dim(y)[2]
        low = up = matrix(, p, 1)
        for (i in 1:p) {
             up[i, ] = quantile(outcurve[, i], probs = 0.75)
             low[i, ] = quantile(outcurve[, i], probs = 0.25)
        }
        IQR = up - low
        dist <- (rob[, 1] - pcbag$center[1])^2 + (rob[, 2] - pcbag$center[2])^2
        center <- order(dist)[1]
        centercurve <- y[center, ]
        notchlow <- centercurve - 1.57 * (IQR) / sqrt(nrow(y))
        notchupper <- centercurve + 1.57 * (IQR) / sqrt(nrow(y))
        n <- length(outlier)
        plot(c(x, rev(x)), c(maximum2, rev(minimum2)), type = "n", 
             main = "", ylim = ylim, xlab = xlab, ylab = ylab, ...)
        polygon(c(x, rev(x)), c(maximum2, rev(minimum2)), border = FALSE, 
                col = "light gray", ylim = ylim, ...)
        polygon(c(x, rev(x)), c(maximum1, rev(minimum1)), border = FALSE, 
                col = "dark gray", ...)
        lines(fts(x, notchlow), col = "blue", lty = 2, ...)
        lines(fts(x, notchupper), col = "blue", lty = 2, ...)
        lines(fts(x, centercurve), col = "black", ...)
        if (n > 0) {
            outliercurve <- y[outlier, ]
            lines(fts(x, t(outliercurve)), col = rainbow(n), ...)
            return(outlier)
        }
   }
}

