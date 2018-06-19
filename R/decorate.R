#decorates ans based on the rownames and colnames of m
decorate_ans<-function(ans, m) {
  for(i in seq_along(ans)) {
    l<-ans[[i]]
    if(!is.null(colnames(m))){
      l$cols<-setNames(l$cols, colnames(m)[l$cols])
    }
    if(!is.null(rownames(m))){
      l$rows<-setNames(l$rows, rownames(m)[l$rows])
    }
    ans[[i]]<-l
  }
  return(ans)
}
