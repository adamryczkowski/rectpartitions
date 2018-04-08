stack.push <- function(stack, new.element){
  temp.stack <- new.env()
  temp.stack$value <- new.element
  if('top.element' %in% names(stack) ) {
    temp.stack$next.element <- stack$top.element
  }
  stack$top.element <- temp.stack
}

stack.pop <- function(stack){
  if(stack.is_empty(stack)) {
    #empty stack
    browser()
  }
  popped_element<-stack$top.element
  value <- popped_element$value
  if('next.element' %in% names(popped_element)) {
    stack$top.element <- popped_element$next.element
  } else {
    rm(top.element, envir=stack)
  }
  return(value)
}

stack.top <- function(stack){
  if(stack.is_empty(stack)) {
    return(NULL)
  } else {
    return(stack$top.element$value)
  }
}

stack.is_empty<-function(stack) {
  !'top.element' %in% names(stack)
}

stack.length<-function(stack) {
  if(stack.is_empty(stack)) {
    return(0)
  } else {
    return(1+stack.length(stack$next.element))
  }
}

stack.to_list<-function(stack) {
  if(stack.is_empty(stack)){
    return(list())
  }
  len<-stack.length(stack)
  ans<-rep(list(list()),len)
  item<-stack$top.element
  i<-1
  while(!is.null(item)) {
    ans[[i]]<-item$value
    item<-item$next.element
  }
  return(ans)
}
