#' Function that tries to partition the matrix of integers into a minimal set of rectangles.
#'
#' @param m Matrix of integer to partition.
#' @param target_value Value to use as a marker. Defaults to "1". Any other value than that will be treated as the absense of marker.
#' @return Returns list of rectangles. Each rectangle is a list defined by \code{x0}, \code{x1}, \code{y0} and \code{y1}.
#' @export
get_rectangles<-function(m, target_value=1) {
  #Algorithm
  #1. Compress matrix to eliminate duplicate rows & cols
  rect<-matrix(as.integer(m==1), nrow = nrow(m), ncol=ncol(m))
  wrect<-compress(rect = rect, flag_ordered = TRUE)
  #2. Find all continuous subsets
  all_parts<-partition_rect(rect = wrect)
  ans_list<-list() #List with all the found rectangles
  while(length(all_parts)>0) {
    new_parts<-list()
    for(part in all_parts) {
      ans<-maxRectangle(rect = part, colweights = attr(part, 'colweights'),
                    rowweights = attr(part, 'rowweights'))
      part<-zero_rect(part, ans)
      if(sum(part)>0) {
        new_parts<-c(new_parts, partition_rect(rect = part))
      }
      ans_list<-c(ans_list, list(ans))
    }
    all_parts<-new_parts
  }
  return(ans_list)
}

#' Function that re-shuffles columns and rows of the integer matrix to find the best partitionioning into a minimal set of rectangles.
#'
#' @param m Matrix of integer to partition.
#' @param target_value Value to use as a marker. Defaults to "1". Any other value than that will be treated as the absense of marker.
#' @param method Shuffling is done by means of hierarchical clustering. This parameter denotes the clustering algorithm to use. Defaults to \code{'single'}.
#' @return Returns list of rectangles. Each rectangle is a list defined by \code{x0}, \code{x1}, \code{y0} and \code{y1}.
#' @export
get_rectangles_shuffle<-function(m, target_value=1, method='single') {
  #Algorithm
  #1. Shuffle rows and columns with the help of the hierarchical clustering
  #2. Perform analysis on the shuffled matrix
  #3. Un-shuffle it
  dists<-dist(m, method='manhattan')
  shuf_y<-hclust(dists, method=method)$order
  rect_y<-m[shuf_y,]

  dists<-dist(t(m), method='manhattan')
  shuf_x<-hclust(dists, method=method)$order
  rect_xy<-rect_y[,shuf_x]
  ans<-get_rectangles(rect_xy)
  ans
}
