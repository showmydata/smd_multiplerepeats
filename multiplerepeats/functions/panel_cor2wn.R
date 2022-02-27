panel_cor2wn <- function(x, y, x1, y1, sp, nvar, lw, ss, smoothness, digits, cohensd, perc_rank, domedian, domean, do95CI, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  data=isolate_complete_pairs(x,y); # isolate complete pairs
  #if (perc_rank) {data=perc_rank(data)} # re-rank data with just these pairs for correct spearman correlations
  # compute correlation coefficient & n
  #r=cor(data[,1],data[,2])
  n=length(data[,1])
  # cis_r=CIr(r, n = n, level = .95)
  # lowerci_r=cis_r[1]
  # upperci_r=cis_r[2]
  thet=t.test(x,y,paired=TRUE)
  sdx=sd(x,na.rm=TRUE); sdy=sd(y,na.rm=TRUE);
  meansd=sqrt((sdx^2+sdy^2)/2)
  #cat(thet$estimate)
  #cat(meansd)
  #cat(thet$estimate/meansd)
  # generate text
  if (cohensd==FALSE) { 
    num1 <- format(c(thet$estimate, 0.123456789), digits = digits)[1] 
    txt9 <- paste("Mean difference = ", sep = "") # n
  } else {
    num1 <- format(c(thet$estimate/meansd, 0.123456789), digits = digits)[1] 
    txt9 <- paste("Mean difference (SDs) = ", sep = "") # n
  }
  if (sp) txt5 <- paste(num1, sep = "") else txt5 <- paste(num1, sep = "") # r or rho
  
  num2 <- format(c(n, 0.123456789), digits = 0, scientific = FALSE)[1] # n text
  txt6 <- paste("n = ", num2, sep = "") # n
  
  txt7 <- paste(" 95% CI =", sep= "") # text introducing the 95% CI
  
  if (cohensd==FALSE) { 
  num3 <- format(c(thet[4]$conf.int[1], 0.123456789), digits = digits)[1] # upper 95% CI text
  num4 <- format(c(thet[4]$conf.int[2], 0.123456789), digits = digits)[1] # lower 95% CI text
  } else {
    num3 <- format(c(thet[4]$conf.int[1]/meansd, 0.123456789), digits = digits)[1] # upper 95% CI text
    num4 <- format(c(thet[4]$conf.int[2]/meansd, 0.123456789), digits = digits)[1] # lower 95% CI text
  }
  txt8 <- paste("[", num3, ", ", num4, "]", sep = "") # 95% CI itself
  
#  txt6 <- paste("t(", thet[2]$parameter, ")=", round(thet[1]$statistic,2), sep = "")
#  txt7 <- paste("p=", round(thet[3]$p.value,3),sep="")
  
  # write text
  text(0.5, 0.80, txt9, cex=.75/(nvar/9), font=2)
  text(0.5, 0.68, txt5, cex=1.25/(nvar/9), font=2)
  text(0.5, 0.50, txt7, cex=.9/(nvar/9))
  text(0.5, 0.37, txt8, cex=.9/(nvar/9))
  text(0.5, 0.20, txt6, cex=1/(nvar/9))
}