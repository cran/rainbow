`plot.fds` <- function (x, plot.type = c("functions", "time", "depth", "density"), col = NULL, type = "l", lty = 1, xlab = x$xname, ylab = x$yname, pch = c(1:9,0, letters, LETTERS), add = FALSE, index, ...)
{
    if (class(x)[1] == "fts"|class(x)[1] == "fds"|class(x)[1] == "sfts"){
        plot.type <- match.arg(plot.type)    
        if (plot.type == "time"){
            if (class(x)[1] == "fts"|class(x)[1] == "sfts"){
                if (is.null(col)) {
                    nx <- length(x$x)
                    col = rainbow(min(1024, 1.25 * nx))
                }
                if (xlab == x$xname){
                    xlab <- "Time"
                }
                year = as.numeric(colnames(x$y))
				if (add == FALSE){
                    matplot(year, t(x$y), type = type, ylab = ylab, 
                            xlab = xlab, col = col, lty = lty, pch = pch, ...)
				}
				else
				{
				    matlines(year, t(x$y)[,index], type = type, ylab = ylab, 
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
				{
                    col <- rainbow(min(1024,1.25 * ny))
			    }
				else
				{
				    col <- 1 
			    }
            }
            yy <- as.matrix(x$y)
            if (plot.type == "depth"){
                sco <- PCAproj(t(yy), k = 2, center = median)$score
                center <- compute.bagplot(sco)$center
                lineindex <- order(mahalanobis(sco, center, cov(sco)))
                yy <- yy[,lineindex]
                yymax <- yy[,1]
            }   
            else if (plot.type == "density"){
                     sco <- PCAproj(t(yy), k = 2, center = median)$score
                     X <- cbind(sco[,1], sco[,2])
                     h = Hscv.diag(X, binned = TRUE)
                     den = kde(x = X, H = h)
                     den = list(x = den$eval.points[[1]], y = den$eval.points[[2]], z = den$estimate)
                     den2 <- hdrcde:::hdr.info.2d(sco[,1], sco[,2], den, alpha = c(0.01,.5))
                     lineindex <- order(den2$fxy, decreasing = TRUE)
                     yy <- yy[,lineindex]
                     yymax <- yy[,1]
            }
            if (nrow(yy) == 1)
                plot(ts(c(yy), s = start(x$time), f = frequency(x$time)),
                     ylab = ylab, ...)        
            if (plot.type == "functions"){
			    if (add == FALSE){
                    matplot(x$x, yy, col = col, xlab = xlab, ylab = ylab, type = type, lty = lty, pch = pch, ...)
				}
                else{
                    matlines(x$x, yy[,index], col = col, xlab = xlab, ylab = ylab, type = type, lty = lty, pch = pch, ...)				    
                }				
            }
            else{      
			    if (add == FALSE){
                    matplot(x$x, yy, col = col, xlab = xlab, ylab = ylab, type = type, lty = lty, pch = pch, ...)
                    lines(x$x, yymax, col = "black", ...)
				}
				else{
				    matlines(x$x, yy[,index], col = col, xlab = xlab, ylab = ylab, type = type, lty = lty, pch = pch, ...)
				}
        }
        }
   }
   else{
        stop("object is not a functional model.")
   }
}
