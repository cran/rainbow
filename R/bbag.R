bbag <- function(data,factor = 2.57,label = TRUE){
  y <- t(data$y)
  sco <- PCAproj(y, k=2)$scores
  tmp <- compute.bagplot(sco[,1],sco[,2],factor = factor,verbose = FALSE)
  plot.bagplot(tmp, col.loophull = gray(.95), col.baghull = gray(.8),
       show.whiskers = FALSE, xlab = "PC score 1", ylab = "PC score 2")
  points(sco[,1], sco[,2], pch = 16, cex = 0.5, col = 1)
  outliers <- as.numeric(rownames(tmp$pxy.outlier))
  points(sco[outliers,1], sco[outliers,2], col = rainbow(length(outliers)),
         pch=16)
  box()
  if (length(outliers)!=0){
      if (label){
          year = as.numeric(rownames(y))
          text(sco[outliers,1]-0.2, sco[outliers,2], year[outliers], adj = 1,
               col = rainbow(length(outliers)))
      }
      return(outliers)
  }
}

