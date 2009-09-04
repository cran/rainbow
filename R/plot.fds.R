`plot.fds` <- function (x, plot.type = c("functions", "time", "depth", "density"), labels = NULL, label.cex = 0.7, 
        col = NULL, type = "l", lty = 1, xlab = x$xname, ylab = x$yname, pch = c(1:9,0, letters, LETTERS), ...)
{
    if (class(x)[1] == "fts"|class(x)[1] == "fds"|class(x)[1] == "sfts"){
        plot.type <- match.arg(plot.type)    
        if (plot.type == "time"){
            if (class(x)[1] == "fts"|class(x)[1] == "sfts"){
                if (is.null(col)) {
                    nx <- length(x$x)
                    palette(rainbow(min(1024, 1.25 * nx)))
                    col = 1:nx
                }
                if (xlab == x$xname){
                    xlab <- "Time"
                    year = as.numeric(colnames(x$y))
                    matplot(year, t(x$y), type = type, ylab = ylab, 
                            xlab = xlab, col = col, lty = lty, pch = pch, ...)
                }
            }
            else{
                stop("object is not a functional time series.")
            }            
        }
        else{
            if (is.null(col)) {
                ny <- ncol(as.matrix(x$y))
                if (ny > 1)
                    palette(rainbow(min(1024,1.25 * ny)))
                    col <- 1:ny
            }
            yy <- as.matrix(x$y)
            if (plot.type == "depth"){
                sco <- PCAproj(t(yy), k = 2)$score
                center <- compute.bagplot(sco)$center
                index <- order(mahalanobis(sco, center, cov(sco)))
                yy <- yy[,index]
                yymax <- yy[,1]
            }   
            else if (plot.type == "density"){
                     sco <- PCAproj(t(yy), k = 2)$score
                     X <- cbind(sco[,1], sco[,2])
                     h = Hscv.diag(X, binned = TRUE)
                     den = kde(x = X, H = h)
                     den = list(x = den$eval.points[[1]], y = den$eval.points[[2]], z = den$estimate)
                     den2 <- hdrcde:::hdr.info.2d(sco[,1], sco[,2], den, alpha = c(0.01,.5))
                     index <- order(den2$fxy, decreasing = TRUE)
                     yy <- yy[,index]
                     yymax <- yy[,1]
            }
            if (nrow(yy) == 1)
                plot(ts(c(yy), s = start(x$time), f = frequency(x$time)),
                     ylab = ylab, ...)        
            if (plot.type == "functions"){
                matplot(x$x, yy, col = col, xlab = xlab, ylab = ylab, type = type, lty = lty, pch = pch, ...)
            }
            else{      
                matplot(x$x, yy, col = col, xlab = xlab, ylab = ylab, type = type, lty = lty, pch = pch, ...)
                lines(x$x, yymax, col = "black", ...)
        }
        palette("default") 
        }
   }
   else{
        stop("object is not a functional model.")
   }
}
