SVDplot <-
function(object, order = 1, center = c("rowwise", "colwise", "double"), plot = TRUE)
{
  center = match.arg(center)
  data = object$y
  p = dim(data)[1]
  n = dim(data)[2]
  leftoutobs = order + 1
  datarowmean = matrix(, p, (n - leftoutobs))
  datacolmean = matrix(, (p - leftoutobs), n)
  svdrow = array(, dim = c(p, (n - leftoutobs), order))
  svdcol = array(, dim = c((p - leftoutobs), n, order)) 
  if (center == "rowwise"){
      newdata = data - rowMeans(data)
  }
  if (center == "colwise"){
      newdata = t(t(data) - colMeans(data))
  }
  if (center == "double"){
      newdata = t(t(data - rowMeans(data)) - colMeans(data)) + mean(data)
  }
  means = data - newdata
  for(i in 1:(n - leftoutobs)){
      datarowmean[,i] = as.matrix(rowMeans(data[, 1:(leftoutobs + i)]))
  }
  for(i in 1:(p - leftoutobs)){
         datacolmean[i,] = as.matrix(colMeans(data[1:(leftoutobs + i), ]))
  }
  if (order > 0){
      for(i in 1:(n - leftoutobs)){
          for(j in 1:order){
              svdrow[,i,j] = abs(svd(t(newdata)[1:(leftoutobs + i), ])$v[,j])
          }
      }
      for(i in 1:(p - leftoutobs)){
          for(j in 1:order){
              svdcol[i,,j] = svd(t(newdata)[, 1:(leftoutobs + i)])$u[,j]
          }
      }
  }
  if (order == 1){
      approx = as.matrix(svd(t(newdata))$v[,1]) %*% (svd(t(newdata))$d[1]*svd(t(newdata))$u[,1]) + means
      resi = newdata - as.matrix(svd(t(newdata))$v[,1]) %*% (svd(t(newdata))$d[1]*svd(t(newdata))$u[,1])
  }
  if (order > 1){
      svdcomb = array(, dim = c(p, n, order))
      for(i in 1:order){
          svdcomb[,,i] = as.matrix(svd(t(newdata))$v[,i]) %*% (svd(t(newdata))$d[i]*svd(t(newdata))$u[,i])
      }
      approx = apply(svdcomb, 1:2, sum) + means
      resi = newdata - apply(svdcomb, 1:2, sum)
  }
  if (plot == TRUE){
      old.par <- par(no.readonly = TRUE)
      if(order == 0){
         par(mfrow = c(1,2))
         plot(fts(as.numeric(rownames(object$y)), datarowmean), xlab = object$xname, ylab = "SVD1")
         plot(fts(as.numeric(colnames(object$y)), t(datacolmean)), xlab = "Time", ylab = "SVD1")
      }
      else{
         par(mfrow = c(2, order + 3))
         plot(fts(as.numeric(rownames(object$y)), datarowmean), xlab = object$xname, ylab = "SVD1")
         for(i in 1:order){
             plot(fts(as.numeric(rownames(object$y)), svdrow[,,i]), xlab = object$xname, ylab = paste("SVD",(i+1), sep = ""))
         }
         plot(fts(as.numeric(rownames(object$y)), approx), xlab = object$xname, ylab = "SVD approximation")
         plot(fts(as.numeric(rownames(object$y)), resi), xlab = object$xname, ylab = "Residual")
         plot(fts(as.numeric(colnames(object$y)), t(datacolmean)), xlab = "Time", ylab = "SVD1")
         for(i in 1:order){
             plot(fts(as.numeric(colnames(object$y)), t(svdcol[,,i])), xlab = "Time", ylab = paste("SVD",(i+1), sep = ""))
         }
         plot(fts(as.numeric(colnames(object$y)), t(approx)), xlab = "Time", ylab = "SVD approximation")
         plot(fts(as.numeric(colnames(object$y)), t(resi)), xlab = "Time", ylab = "Residual")
      }
      on.exit(par(old.par)) 
  }
  else {
       if (order == 0){
           return(list(datarowmean = datarowmean, datacolmean = datacolmean, xname = object$xname, yname = object$yname))
       }
       else{
           return(list(datarowmean = datarowmean, datacolmean = datacolmean, svdrow = svdrow, svdcol = svdcol,
                  approx = approx, residual = resi, xname = object$xname, yname = object$yname))
       }
  }
}

