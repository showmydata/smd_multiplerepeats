my_points_rm <- function(x, y, x1, y1, sp, nvar, lw, ss, smoothness, digits, cohensd, perc_rank, domedian, domean, do95CI, dotint, panelcolor, tintmaxdiff, ...){
  data=isolate_complete_pairs(x,y); 
  x=data[,1]; y=data[,2];
  xmean=mean(x, na.rm = TRUE); ymean=mean(y, na.rm = TRUE); 
  meandiffs=abs(xmean-ymean);
  if (dotint==1) {
    a=meandiffs/tintmaxdiff; if (a>=1) a=.99 else if (a<=-1) a=-.99 
    b=col2rgb(panelcolor)/255; 
    c=rgb(b[1],b[2],b[3],alpha=abs(a)); 
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); 
  }
  #points(x1,y1,...)
  segments(x0=numeric(length(x1))+.5, y0=x1, x1=numeric(length(x1))+2.5, y1=y1)
  abline(0,1,lwd=lw)
  if (domedian) {xmedian=median(x); ymedian=median(y); points(xmedian,ymedian,pch=16,col="purple",cex=ss)}
  if (domean) {xmean=mean(x,na.rm=TRUE); ymean=mean(y,na.rm=TRUE); points(xmean,ymean,pch=16,col="red",cex=ss)}
  if (do95CI) {
    diffs=x-y
    ci=(sd(diffs)/sqrt(length(diffs)))*1.96
    lines(c(xmean,xmean),c(ymean-ci,ymean+ci),col="red",lwd=lw)
  }
}

my_line_rm <- function(x, y, x1, y1, sp, nvar, lw, ss, smoothness, digits, cohensd, perc_rank, domedian, domean, do95CI, dotint, panelcolor, tintmaxdiff, ...){
  data=isolate_complete_pairs(x,y); 
  x=data[,1]; y=data[,2];
  xmean=mean(x, na.rm = TRUE); ymean=mean(y, na.rm = TRUE); 
  meandiffs=abs(xmean-ymean);
  if (dotint==1) {
    a=meandiffs/tintmaxdiff; if (a>=1) a=.99 else if (a<=-1) a=-.99 
    b=col2rgb(panelcolor)/255; 
    c=rgb(b[1],b[2],b[3],alpha=abs(a)); 
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); 
  }
  points(x1,y1,...)
  abline(0,1,lwd=lw)
  abline(lm(y~x),lwd=lw,col="blue")
  if (domedian) {xmedian=median(x); ymedian=median(y); points(xmedian,ymedian,pch=16,col="purple",cex=ss)}
  if (domean) {xmean=mean(x,na.rm=TRUE); ymean=mean(y,na.rm=TRUE); points(xmean,ymean,pch=16,col="red",cex=ss)}
  if (do95CI) {
    diffs=x-y
    ci=(sd(diffs)/sqrt(length(diffs)))*1.96
    lines(c(xmean,xmean),c(ymean-ci,ymean+ci),col="red",lwd=lw)
  }
}

my_curve_rm <- function(x, y, x1, y1, sp, nvar, lw, ss, smoothness, digits, cohensd, perc_rank, domedian, domean, do95CI, dotint, panelcolor, tintmaxdiff, ...){
  data=isolate_complete_pairs(x,y);   
  x=data[,1]; y=data[,2];
  xmean=mean(x, na.rm = TRUE); ymean=mean(y, na.rm = TRUE); 
  meandiffs=abs(xmean-ymean);
  if (dotint==1) {
    a=meandiffs/tintmaxdiff; if (a>=1) a=.99 else if (a<=-1) a=-.99 
    b=col2rgb(panelcolor)/255; 
    c=rgb(b[1],b[2],b[3],alpha=abs(a)); 
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); 
  }
  points(x1,y1,...)
  abline(0,1,lwd=lw)
  if(length(unique(x))>4) { 
    data=cbind(x,y)
    smoothingSpline = smooth.spline(na.omit(data), spar=smoothness, tol=.1)
    lines(smoothingSpline,lw=lw,col="darkgreen") 
  }
  if (domedian) {xmedian=median(x); ymedian=median(y); points(xmedian,ymedian,pch=16,col="purple",cex=ss)}
  if (domean) {xmean=mean(x,na.rm=TRUE); ymean=mean(y,na.rm=TRUE); points(xmean,ymean,pch=16,col="red",cex=ss)}
  if (do95CI) {
    diffs=x-y
    ci=(sd(diffs)/sqrt(length(diffs)))*1.96
    lines(c(xmean,xmean),c(ymean-ci,ymean+ci),col="red",lwd=lw)
  }
}

my_lineandcurve_rm <- function(x, y, x1, y1, sp, nvar, lw, ss, smoothness, cohensd, digits, perc_rank, domedian, domean, do95CI, dotint, panelcolor, tintmaxdiff, ...){
  data=isolate_complete_pairs(x,y); 
  x=data[,1]; y=data[,2];
  xmean=mean(x, na.rm = TRUE); ymean=mean(y, na.rm = TRUE); 
  meandiffs=abs(xmean-ymean);
  if (dotint==1) {
    a=meandiffs/tintmaxdiff; if (a>=1) a=.99 else if (a<=-1) a=-.99 
    b=col2rgb(panelcolor)/255; 
    c=rgb(b[1],b[2],b[3],alpha=abs(a)); 
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); 
  }
  points(x1,y1,...)
  abline(0,1,lwd=lw)
  abline(lm(y~x),lwd=lw,col="blue")
  if(length(unique(x))>4) {
    data=cbind(x,y)
    smoothingSpline = smooth.spline(na.omit(data), spar=smoothness, tol=.1)
    lines(smoothingSpline,lw=lw,col="darkgreen")
  }
  if (domedian) {xmedian=median(x); ymedian=median(y); points(xmedian,ymedian,pch=15,col="purple",cex=ss)}
  if (domean) {xmean=mean(x); ymean=mean(y); points(xmean,ymean,pch=17,col="red",cex=ss)}
  if (do95CI) {
    xmean=mean(x); ymean=mean(y); 
    diffs=x-y
    ci=(sd(diffs)/sqrt(length(diffs)))*1.96
    lines(c(xmean,xmean),c(ymean-ci,ymean+ci),col="red",lwd=lw)
  }
}