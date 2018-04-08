partition_rect<-function(rect, flag_ordered = TRUE) {
  ans<-list()
  atrs<-attributes(rect)
  while(TRUE) {
    point<-find_first_non_zero(rect)
    if(length(point)==0) break
    obj<-partition_rect_1(rect, point$col, point$row)
    rect<-obj$rect
    attributes(obj$target_rect)<-atrs
#    browser()
    obj$target_rect<-compress(rect = obj$target_rect, flag_ordered = flag_ordered)

    ans<-c(ans, list(obj$target_rect))
  }
  return(ans)
}

#Returns continuous subpartitions.
#The function simulates walk, with each step adds
partition_rect_1<-function(rect, x, y) {
  rect<-rect #make sure we have a copy
  target_rect<-as.integer(rep(0, length(rect)))
  dim(target_rect)<-dim(rect)
  stack<-new.env()
  stack.push(stack , list(col=x, row=y))
  while(!stack.is_empty(stack)) {
    step<-stack.pop(stack)
    x<-step$col
    y<-step$row
    rect[y, x]<-as.integer(0)
    target_rect[y, x]<-as.integer(1)
    if(x>1) {
      if(rect[y, x-1]!=0) {
        stack.push(stack, list(col=x-1, row=y))
      }
    }
    if(y>1) {
      if(rect[y-1, x]!=0) {
        stack.push(stack, list(col=x, row=y-1))
      }
    }
    if(x<ncol(rect)) {
      if(rect[y, x+1]!=0) {
        stack.push(stack, list(col=x+1, row=y))
      }
    }
    if(y<nrow(rect)) {
      if(rect[y+1, x]!=0) {
        stack.push(stack, list(col=x, row=y+1))
      }
    }
  }
  return(list(rect=rect, target_rect=target_rect))
}
