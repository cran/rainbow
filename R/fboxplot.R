fboxplot <- function (data, plot.type = c("functional", "bivariate"), type = c("bag", 
    "hdr"), alpha = c(0.01, 0.5), h, factor = 2.57, na.rm = TRUE,
    xlab = data$xname, ylab = data$yname, ...) 
{
    op <- par(no.readonly = TRUE)
    type <- match.arg(type)
    plot.type <- match.arg(plot.type)
    y = t(data$y)
    x = data$x
    if (na.rm) 
        y <- na.omit(y)
    if (plot.type == "functional") {
        if (type == "bag") 
            fbag(data, factor, xlab = xlab, ylab = ylab, ...)
        else fhdr(data, alpha, h, xlab = xlab, ylab = ylab, ...)
    }
    if (plot.type == "bivariate") {
        par(pty="s")
        sco = PCAproj(t(data$y))$scores 
        if (type == "bag") 
            bbag(data, factor, ...)
        else bhdr(data, alpha, h, ...)
        exit.restore <- function()
        {
          par(op)
        }
        on.exit(exit.restore()) 
    }
}

