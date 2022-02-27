jitter_by_percent_min_wn <- function(data, perc_jitter) {
  v=colnames(data)
  for (i in 1:ncol(data)) {
    x=data[,i]                                               # grab a column of data
    xjprop=(perc_jitter-1)/200                               # compute proportion of min dot diff to jitter
    xuniq=unique(x)                                          # find unique values
    minxd=min(diff(xuniq[order(xuniq)]),na.rm=TRUE);         # find minimum difference b/w ordered unique values
    if(xjprop>=0) {                                          # if percent jitter is greater than zero then do jitter
      x1=jitter(x,amount=xjprop*minxd) 
    } 
    else x1=x
    data[,i]=x1
  }
  return(data)
}