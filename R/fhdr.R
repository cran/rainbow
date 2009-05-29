fhdr <- function (data, alpha = c(0.01, 0.5), h, label = T, xlab, ylab, ...) 
{
    y = t(data$y)
    sco = PCAproj(y, k = 2)$scores
    ylim = range(y, na.rm = TRUE)
    if (missing(h)) 
        h <- Hscv.diag(sco, binned = TRUE)
    else h <- diag(h)
    den <- kde(x = sco, H = h)
    den <- list(x = den$eval.points[[1]], y = den$eval.points[[2]], 
        z = den$estimate)
    hdr1 <- hdrcde:::hdr.info.2d(sco[, 1], sco[, 2], den, alpha = alpha)
    index <- hdr1$fxy <= min(hdr1$falpha)
    outliers <- which(as.vector(index))
    insideone = which(hdr1$fxy > hdr1$falpha[2])
    insidetwo = which(hdr1$fxy > hdr1$falpha[1])
    center <- which(hdr1$fxy == max(hdr1$fxy))
    insideonecurve = y[insideone, ]
    insidetwocurve = y[insidetwo, ]
    outliercurve = y[outliers, ]
    centercurve <- y[center, ]
    maximum3 <- apply(insideonecurve, 2, max)
    minimum3 <- apply(insideonecurve, 2, min)
    maximum4 <- apply(insidetwocurve, 2, max)
    minimum4 <- apply(insidetwocurve, 2, min)
    n <- length(outliers)
    x = data$x
    plot(c(x, rev(x)), c(maximum4, rev(minimum4)), type = "n", 
        main = "", ylim = ylim, xlab = xlab, ylab = ylab, ...)
    polygon(c(x, rev(x)), c(maximum4, rev(minimum4)), border = FALSE, 
        col = "dark gray")
    polygon(c(x, rev(x)), c(maximum3, rev(minimum3)), border = FALSE, 
        col = "light gray")
    lines(fts(x, centercurve), col = "black", ...)
    if (n > 0) {
        outliercurve <- y[outliers, ]
        lines(fts(x, matrix(t(outliercurve), length(x), n)), 
            col = rainbow(n), ...)
        return(outliers)
    }
}
