`print.fts` <- function (x, digits = NULL, ...) 
{
    cat("Functional time series")
    cat(paste("\n y:", x$yname))
    cat(paste("\n x:", x$xname, "\n"))
}

