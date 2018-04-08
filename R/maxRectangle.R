#https://www.geeksforgeeks.org/maximum-size-rectangle-binary-sub-matrix-1s/


maxRectangle<-function(rect, colweights, rowweights) {
  # Calculate area for first row and initialize it as result
  #debugonce(maxHist)
  wrect<-rect * rowweights
  flagrect<-rect
  best_result <- maxHist(wrect[1,], colweights)
  best_result <- c(best_result, list(y0=1, y1=1))

  if(nrow(rect)==1) {
    return(best_result)
  }

  # iterate over row to find maximum rectangular area considering each row as histogram
  for(y in seq(2, nrow(rect))) {
    wrect[y,]<-(wrect[y,]+wrect[y-1,])*sign(rect[y,])
    flagrect[y,]<-(flagrect[y,]+flagrect[y-1,])*sign(rect[y,])
    result <- maxHist(wrect[y,], colweights)
    if(result$area > best_result$area) {
      best_result<-result
      y0<-y-min(flagrect[y,seq(best_result$x0, best_result$x1)])+1
      best_result <- c(best_result, list(y0=y0, y1=y))
    }
  }
  return(best_result)
}
calc_area<-function(colweights, rowweights) {
  sum(outer(colweights, rowweights, FUN = `*`))
}

zero_rect<-function(part, ans) {
#  browser()
#  which_x<-which(attr(part, 'colmap') %in% seq(ans$x0, ans$x1))
#  which_y<-which(colmap<-attr(part, 'rowmap') %in% seq(ans$y0, ans$y1))
  which_x<-seq(ans$x0, ans$x1)
  which_y<-seq(ans$y0, ans$y1)
  part[which_y,which_x]<-0
  return(part)
}
