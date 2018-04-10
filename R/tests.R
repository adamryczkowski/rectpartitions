gen_test<-function(size_x, size_y, proc=0.2, exp_n=4) {
  m<-matrix(0, ncol=size_x, nrow=size_y)

  rand_one_rect<-function(){
    log_aspect_ratio<-rnorm(1,0,1) #Positive -> horizontal rect
    size_proc<-runif(1, 0, proc/exp_n) #Target size of the rectangle in terms of the total size of the area

    target_area<-size_y * size_x * size_proc #Area of the generated rectangle
    target_x <- round(sqrt(target_area / exp(log_aspect_ratio)))
    target_y <- round(sqrt(target_area * exp(log_aspect_ratio)))

    if(target_x * target_y > 0) {
      pos_x<-floor(runif(1, 0, size_x - target_x))
      pos_y<-floor(runif(1, 0, size_y - target_y))
      return(list(x0=pos_x, x1=pos_x+target_x-1, y0=pos_y, y1=pos_y+target_y-1))
    } else {
      return(list())
    }
  }
  ans_list<-list()
  while(sum(m)<=size_x * size_y * proc) {
    ans<-rand_one_rect()
    if(length(ans)>0) {
      m[seq(ans$y0,ans$y1), seq(ans$x0,ans$x1)]<-1
      ans_list<-c(ans_list, list(ans))
    }
  }
  return(list(m=m, ans_list=ans_list))
}

