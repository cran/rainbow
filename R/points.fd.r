points.fd <- function (x, plot.type = c("functions", "time", "depth", "density"), index, labels = NULL, 
                      label.cex = 0.7, col = NULL, pch = 1, ...) 
{
    plot.type <- match.arg(plot.type)
    if (plot.type == "time") {
        if (class(x)[1] == "fts"){
            if (is.null(col)) {
                nx <- length(x$x)
                palette(rainbow(nx))
                col = 1:nx
            }
            year = as.numeric(colnames(x$y))
            matpoints(year, ts(t(x$y)[,index], s = start(x$time), f = frequency(x$time)), 
                   col = col, pch = pch, ...)
            palette("default")
            if (!is.null(labels)) {
                xlim <- range(x$time)
                text(max(x$time) + (xlim[2] - xlim[1]) * 0.02, x$y[, 
                     ncol(x$y)], labels, adj = 0, cex = label.cex)
            }
        }
        else {
              warning("object is not a functional time series")
        }
    }
    else {
        if (is.null(col)) {
            ny <- ncol(as.matrix(x$y))
            if (ny > 1) 
                palette(rainbow(1.25 * ny))
            col <- 1:ny
        }
        if (plot.type == "functions"){
            matpoints(x$x, x$y[,index], col = col, pch = pch, ...)
        }
        yy <- as.matrix(x$y)
        if (plot.type == "depth"){
            sco <- PCAproj(t(yy),k = 2)$score
            center <- compute.bagplot(sco)$center
            dist <- order(mahalanobis(sco,center,cov(sco)))
            matpoints(x$x, yy[,which(dist == index)], col = col, pch = pch, ...)
        }   
        if (plot.type == "density"){
            sco <- PCAproj(t(yy),k = 2)$score
            X <- cbind(sco[,1],sco[,2])
            h = Hscv.diag(X, binned = TRUE)
            den = kde(x = X, H = h)
            den = list(x=den$eval.points[[1]], y=den$eval.points[[2]], z=den$estimate)
            den2 <- hdrcde:::hdr.info.2d(sco[,1], sco[,2], den, alpha=c(0.01,0.5))
            dist <- order(den2$fxy,decreasing = TRUE)
            matpoints(x$x, yy[,which(dist == index)], col = col, pch = pch, ...)
        }
    }
}

