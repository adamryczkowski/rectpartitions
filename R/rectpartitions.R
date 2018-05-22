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
  ans_list<-decorate_ans(ans_list, m)
  return(ans_list)
}

#' Function that re-shuffles columns and rows of the integer matrix to find the best partitionioning into a minimal set of rectangles.
#'
#' @param m Matrix of integer to partition.
#' @param target_value Value to use as a marker. Defaults to "1". Any other value than that will be treated as the absense of marker.
#' @param method Shuffling is done by means of hierarchical clustering. This parameter denotes the clustering algorithm to use. Defaults to \code{'single'}.
#' @return Returns list of rectangles. Each rectangle is a list defined by \code{x0}, \code{x1}, \code{y0} and \code{y1}.
#' @export
get_rectangles_shuffle<-function(m, target_value=1, method='single', flag_allow_stripes=FALSE) {
  ans_list<-list() #List with all the found rectangles
  rect<-matrix(as.integer(m==target_value), nrow = nrow(m), ncol=ncol(m))

  while(TRUE) {
    if(flag_allow_stripes) {
      sum_horiz<-plyr::aaply(rect, 1, sum)
      sum_vert<-plyr::aaply(rect, 2, sum)
      best_stripe_size<-max(c(sum_horiz, sum_vert))
    } else {
      best_stripe_size<-0
    }

    #Algorithm (in a loop)
    #1. Shuffle rows and columns with the help of the hierarchical clustering
    dists<-dist(rect, method='manhattan')
    shuf_y<-hclust(dists, method=method)$order
    rect_y<-rect[shuf_y,]

    dists<-dist(t(rect), method='manhattan')
    shuf_x<-hclust(dists, method=method)$order
    rect_xy<-rect_y[,shuf_x]
    attr(rect_xy, 'rowmap')<-Matrix::invPerm(shuf_y)
    attr(rect_xy, 'rowweights')<-rep(1, nrow(rect))
    attr(rect_xy, 'colmap')<-Matrix::invPerm(shuf_x)
    attr(rect_xy, 'colweights')<-rep(1, ncol(rect))


    #2. Compress matrix to eliminate duplicate rows & cols
    #debugonce(rectpartitions:::compress)
    wrect<-rectpartitions:::compress(rect = rect_xy, flag_ordered = FALSE)
    #3. Find all continuous subsets
    #debugonce(rectpartitions:::partition_rect)
    all_parts<-rectpartitions:::partition_rect(rect = wrect, flag_ordered = FALSE)
    #4. Perform analysis on the shuffled matrix

    results<-purrr::map(all_parts, ~rectpartitions:::maxRectangle(rect = ., colweights = attr(., 'colweights'),
                                                                  rowweights = attr(., 'rowweights')))

    #5. Get the biggest one
    counts<-purrr::map_dbl(results, 'area')
    if(max(counts)<best_stripe_size) {
      if(max(sum_vert) > max(sum_horiz)) {
        which_n<-which.max(sum_vert)
        ans<-list(cols=which_n, rows=which(rect[,which_n]==1))
        rect[,which_n]<-0
      } else {
        which_n<-which.max(sum_horiz)
        ans<-list(rows=which_n, cols=which(rect[which_n,]==1))
        rect[which_n,]<-0
      }
      ans_list<-c(ans_list,list(ans))
    } else {
      biggest_part_no<-which.max(counts)
      biggest_part<-all_parts[[biggest_part_no]]
      ans<-results[[biggest_part_no]]
      #6. Store the result in an uncompressed way
      ans<-rectpartitions:::gen_ans(ans=ans, rect=biggest_part)
      ans_list<-c(ans_list, list(ans))
      #7. Substract the removed pieces

      rect<-rectpartitions:::zero_rect2(rect, ans)
    }
    if(sum(rect)==0) {
      ans_list<-decorate_ans(ans_list, m)
      return(ans_list)
    } else {
      cat(paste0(sum(rect), " pieces left.\n"))
    }
    #8. Uncompress & un-shuffle
    #9. Un-shuffle it
    #    rect<-rect_xy[Matrix::invPerm(shuf_y),Matrix::invPerm(shuf_x)]
  }
  ans_list<-decorate_ans(ans_list, m)
  return(ans_list)
}

get_stripes_shuffle<-function(m, target_value=1, flag_horiz=TRUE, flag_vert=TRUE) {
  rect<-matrix(as.integer(m==target_value), nrow = nrow(m), ncol=ncol(m))
  ans_list<-list()
  while(sum(rect)>0) {
    if(flag_horiz) {
      sum_horiz<-plyr::aaply(rect, 1, sum)
    } else {
      sum_horiz<-0
    }
    if(flag_vert) {
      sum_vert<-plyr::aaply(rect, 2, sum)
    } else {
      sum_vert<-0
    }
    if(sum(sum_vert) + sum(sum_horiz)==0) {
      break
    }
    if(max(sum_vert) > max(sum_horiz)) {
      which_n<-which.max(sum_vert)
      ans<-list(cols=which_n, rows=which(rect[,which_n]==1))
      rect[,which_n]<-0
    } else {
      which_n<-which.max(sum_horiz)
      ans<-list(rows=which_n, cols=which(rect[which_n,]==1))
      rect[which_n,]<-0
    }
    ans_list<-c(ans_list,list(ans))
  }
  ans_list<-decorate_ans(ans_list, m)
  return(ans_list)
}

gen_ans<-function(ans, rect) {
  rows<-seq(ans$y0, ans$y1)
  cols<-seq(ans$x0, ans$x1)
  rows<-unlist(purrr::map(rows, ~which(attr(rect,"rowmap")==.)))
  cols<-unlist(purrr::map(cols, ~which(attr(rect,"colmap")==.)))
  return(list(rows=rows, cols=cols))
}
