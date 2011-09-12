fboxplot = function (data, plot.type = c("functional", "bivariate"), type = c("bag", 
    "hdr"), alpha = c(0.01, 0.5), factor = 2.57, na.rm = TRUE, 
    xlab = data$xname, ylab = data$yname, shadecols = gray((9:1)/10), 
    pointcol = 1, plotlegend = TRUE, legendpos = "topright", ncol=2, ...) 
{
    op <- par(no.readonly = TRUE)
    type <- match.arg(type)
    plot.type <- match.arg(plot.type)
    y = t(data$y)
    if (na.rm) 
        y <- na.omit(y)
    if (plot.type == "functional") {
        if (type == "bag") 
            fbag(data, factor, xlab = xlab, ylab = ylab, plotlegend = plotlegend, legendpos = legendpos, ncol=ncol, ...)
        else fhdr(data, alpha, xlab = xlab, ylab = ylab, plotlegend = plotlegend, legendpos = legendpos, ncol=ncol, ...)
    }
    if (plot.type == "bivariate") {
        par(pty = "s")
        sco = PCAproj(t(data$y), center = median)$scores
        if (type == "bag") 
            bbag(data, factor, ...)
        else bhdr(data, alpha, shadecols = shadecols, pointcol = pointcol,...)
        exit.restore <- function() {
            par(op)
        }
        on.exit(exit.restore())
    }
}
