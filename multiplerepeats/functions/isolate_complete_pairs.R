isolate_complete_pairs <- function(x, y) {
# remove non-numeric data
# changes to character in case R took strings & made them into factors, 
# then coerces to be numbers (except NA and NaN which are kept but in 
# a form that both are considered by is.na to be NA)
newx=as.numeric(as.character(x)) 
newy=as.numeric(as.character(y)) 
# keep everything except for NA (or NaN)
keepx=!is.na(newx) 
keepy=!is.na(newy) 
keep=keepx & keepy
# put kept data only into single data frame
z=cbind(newx[keep],newy[keep]); 
return(z)
}