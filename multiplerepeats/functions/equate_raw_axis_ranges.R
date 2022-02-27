equate_raw_axis_ranges <- function(data, cushion=.1, ...){
  # Selects axis ranges that all span the same raw range 
  mins=apply(data,2,min,na.rm=TRUE); 
  maxs=apply(data,2,max,na.rm=TRUE); 
  minmins=min(mins)
  maxmaxs=max(maxs)
  cushion=(maxmaxs-minmins)*cushion
  minmins[1:ncol(data)]=minmins-cushion*.5
  maxmaxs[1:ncol(data)]=maxmaxs+cushion*.5
  ranges=cbind(minmins,maxmaxs)
  return(ranges)
}
