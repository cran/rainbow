bhdr <- function (data, alpha = c(0.01, 0.5), h, label = T) 
{
    y = t(data$y)
    sco = PCAproj(y, k = 2)$scores
    if (missing(h)) 
        h <- Hscv.diag(sco, binned = TRUE)
    else h <- diag(h)
        den <- kde(x = sco, H = h)
        den <- list(x = den$eval.points[[1]], y = den$eval.points[[2]], 
            z = den$estimate)
    hdr1 <- hdrcde:::hdr.info.2d(sco[, 1], sco[, 2], den, alpha = alpha)
    hdrcde:::plothdr2d(sco[, 1], sco[, 2], den, alpha, xlab = "PC score 1", 
        ylab = "PC score 2", show.points = FALSE, , xaxs = "i", 
        yaxs = "i")
    points(sco[, 1], sco[, 2], pch = 16, cex = 0.5, col = 1)
    points(hdr1$mode[1], hdr1$mode[2], pch = 8, col = "red")
    index <- hdr1$fxy <= min(hdr1$falpha)
    outliers <- which(as.vector(index))
    points(sco[outliers, 1], sco[outliers, 2], col = rainbow(length(outliers)), 
        pch = 16)
    if (label) {
        year = as.numeric(rownames(y))
        text(sco[outliers, 1] - 0.2, sco[outliers, 2], year[outliers], 
            adj = 1, col = rainbow(length(outliers)))
    }
    return(outliers)
}


