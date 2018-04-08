#function find all identical colums and rows and groups them together, adding back reference
#that allows to reconstruct the original matrix
compress<-function(rect, flag_ordered=TRUE) {
  if(!'integer' %in% class(rect)) {
    a<-as.integer(rect)
    dim(a)<-dim(rect)
    rect<-a
  }
  checkmate::assertArray(rect, mode='integer', any.missing = FALSE, d=2, null.ok=FALSE)
  testthat::expect_gte(min(rect),0)
  testthat::expect_lte(max(rect),1)
  testthat::expect_lte(length(unique(as.integer(rect))),2)
  checkmate::assert_flag(flag_ordered)

  rect<-compress_1(rect, flag_ordered, flag_col=TRUE)
  rect<-compress_1(rect, flag_ordered, flag_col=FALSE)
  return(rect)
}

#Function recreates compressed matrix
uncompress<-function(rect) {
  colmap<-attr(rect, 'colmap')
  rect1<-uncompress_1(rect, colmap)
  rowmap<-attr(rect, 'rowmap')
  rect2<-t(uncompress_1(t(rect1), rowmap))
  return(rect2)
}

uncompress_1<-function(rect, colmap) {
#  l<-length(colmap)
  ans<-rect[,colmap,drop=FALSE]
  return(ans)
}

#Finds duplicates of either colums or rows.
compress_1<-function(rect, flag_ordered, flag_col) {
#  browser()
  if(flag_col) {
    if(ncol(rect)<2) {
      ans<-rect
      attr(ans, 'colweights')<-1L
      attr(ans, 'colmap')<-1L
      attr(ans, 'rowweights')<-attr(rect, 'rowweights')
      attr(ans, 'rowmap')<-attr(rect, 'rowmap')
      return(ans)
    }
    items<-purrr::map_chr(seq_len(ncol(rect)), ~paste0(rect[,.], collapse=''))
    input_weights<-attr(rect, 'colweights')
    if(is.null(input_weights)) {
      input_weights<-rep(1, ncol(rect))
    }
    input_map<-attr(rect, 'colmap')
    if(is.null(input_map)) {
      input_map<-seq_len(ncol(rect))
    }

  } else {
    if(nrow(rect)<2) {
      ans<-rect

      attr(ans, 'rowweights')<-1L
      attr(ans, 'rowmap')<-1L
      attr(ans, 'colweights')<-attr(rect, 'colweights')
      attr(ans, 'colmap')<-attr(rect, 'colmap')
      return(ans)
    }
    input_weights<-attr(rect, 'rowweights')
    if(is.null(input_weights)) {
      input_weights<-rep(1, nrow(rect))
    }
    input_map<-attr(rect, 'rowmap')
    if(is.null(input_map)) {
      input_map<-seq_len(nrow(rect))
    }
    items<-purrr::map_chr(seq_len(nrow(rect)), ~paste0(rect[.,], collapse=''))
  }
  dup_cols<-seq_along(items) #Points to the original column. At the beginning all columns are original to themselves
  dup_idx<-1
  weights<-rep(1, length(items))
  if(flag_ordered) {
    for(i in seq(2, length(items))) {
      ref<-dup_cols[[i-1]]
      if(items[[i]]==items[[ref]]) {
        dup_cols[[i]] <- ref
        weights[[ref]]<-weights[[ref]]+1
        weights[[i]]<-0
      }
    }
  } else {
    for(i in seq(2, length(items))) {
      refs<-unique(dup_cols[seq(1, i-1)])
      pos<-which(items[refs] == items[[i]])
      if(length(pos)>0) {
        dup_cols[[i]] <- refs[[pos]]
        weights[[refs[[pos]] ]]<-weights[[refs[[pos]] ]]+1
        weights[[i]]<-0
      }
    }
  }
  weights<-purrr::map_dbl(seq_along(weights), ~sum(input_weights[dup_cols==.]))
  dup_cols<-dup_cols[input_map]

  if(flag_col) {
    ans<-rect[,weights>0, drop=FALSE]

    attr(ans, 'colweights')<-weights[weights>0,drop=FALSE]
    attr(ans, 'colmap')<-as.integer(as.factor(dup_cols))
    attr(ans, 'rowweights')<-attr(rect, 'rowweights')
    attr(ans, 'rowmap')<-attr(rect, 'rowmap')

  } else {

    ans<-rect[weights>0,,drop=FALSE]

    attr(ans, 'rowweights')<-weights[weights>0,drop=FALSE]
    attr(ans, 'rowmap')<-as.integer(as.factor(dup_cols))
    attr(ans, 'colweights')<-attr(rect, 'colweights')
    attr(ans, 'colmap')<-attr(rect, 'colmap')
  }
  return(ans)
}
