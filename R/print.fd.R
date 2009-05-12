`print.fd` <- function (x, digits = NULL, ...) 
{
    cat("Functional data set")
    cat(paste("\n y:", x$yname))
    cat(paste("\n x:", x$xname, "\n"))
}

