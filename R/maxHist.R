#https://www.geeksforgeeks.org/maximum-size-rectangle-binary-sub-matrix-1s/

# Finds the maximum area under the histogram represented
# by histogram.  https://www.geeksforgeeks.org/largest-rectangle-under-histogram/
maxHist<-function(row, colweights) {
  #checkmate::asInteger(row)
  # Create an empty stack. The stack holds indexes of hist[] array and theyir area.
  # The bars stored in stack are always in increasing order of their heights.
  result<-new.env()
  best_result<-list()
  top_val<-0L #Top of stack
  max_area<--1L #Max area in current row (or histogram)
  area<-0L #Area with current top
  i<-1
  while(i<=length(row)) {
    if(stack.is_empty(result) || row[[stack.top(result)]] <= row[[i]]) {
      # If this bar is higher than the bar on top stack, push it to stack
      #stack.push(result,  list(x0=x0, x1=i - 1, area=area))
      stack.push(result, i)
      i<-i + 1;
    } else {
      # If this bar is lower than top of stack, then calculate area of rectangle with stack top as
      # the smallest (or minimum height) bar. 'i' is 'right index' for the top and element before
      # top in stack is 'left index'
      x1<-stack.pop(result)
      top_val<-row[[x1]]
      if (stack.is_empty(result)) {
        x0<-1
      } else {
        x0<-stack.top(result)+1
      }
      area <- top_val * (i - x0)

#      area <- res$area
      if(area>max_area) {
        warea <- calc_area(colweights[seq(x0, i-1)], top_val)
        best_result<-list(x0=x0, x1=i-1, area=warea)
        max_area <- area
      }
    }
  }

  # Now pop the remaining bars from stack and calculate area
  # with every popped bar as the smallest bar

  while(!stack.is_empty(result)) {
    x1<-stack.pop(result)
    top_val<-row[[x1]]
    if (stack.is_empty(result)) {
      x0<-1
    } else {
      x0<-stack.top(result)+1
    }
    area <- top_val * (length(row) - x0 + 1)
    if(area > max_area) {
      warea <- calc_area(colweights[seq(x0, length(row))], top_val)
      best_result<-list(x0=x0, x1=length(row), area=warea)
      max_area<-area
    }
  }
  return(best_result)
}

