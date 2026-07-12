#  File src/library/graphics/R/pairs.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
#  Some parts  Copyright (C) 1999 Dr. Jens Oehlschlaegel-Akiyoshi
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

pairs2wn <- function(x, ...) UseMethod("pairs2wn")

pairs2wn.formula <-
  function(formula, data = NULL, ..., subset, na.action = stats::na.pass)
  {
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action # force in even if  default
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    pairs2wn(mf, ...)
  }

#################################################
## some of the changes are from code
## Copyright (C) 1999 Dr. Jens Oehlschlaegel-Akiyoshi
## Others are by BDR and MM
## This version distributed under GPL (version 2 or later)
#################################################

pairs2wn.default <-
  function (x, jdata=NULL, labels, panel = points, ..., horInd = 1:nc, verInd = 1:nc,
            lower.panel = panel, upper.panel = panel,
            diag.panel = NULL, text.panel = textPanel,
            label.pos = 0.5 + has.diag/3, line.main = 3,
            cex.labels = NULL, font.labels = 1,
            row1attop = TRUE, gap = 1, log = "",
            horOdd = !row1attop, verOdd = !row1attop,
            xlim=NULL, ylim=NULL, 
            sp=NULL, nvar=ncol(x),
            lw=NULL, ss=NULL,
            smoothness=NULL,
            adj=NULL, # a hack to adjust the positioning of the bottom tick label to equal the left tick label
            digits=NULL, 
            cohensd=NULL, perc_rank=NULL,
            domedian = TRUE, domean = TRUE, do95CI = TRUE,
            ticks=NULL,
            dotint=NULL, panelcolor=NULL, tintmaxdiff=NULL,
            showp=NULL) 
  {
    if(doText <- missing(text.panel) || is.function(text.panel))
      textPanel <-
        function(x = 0.5, y = 0.5, txt, cex, font) text(x, y, txt, cex = cex, font = font)
    
    localAxis <- function(side, x, y, xpd, bg, col=NULL, main, oma, ...) {
      ## Explicitly ignore any color argument passed in as
      ## it was most likely meant for the data points and
      ## not for the axis.
      xpd <- NA
      if(side %% 2L == 1L && xl[j]) xpd <- FALSE
      if(side %% 2L == 0L && yl[i]) xpd <- FALSE
      if ((i==1 & j==1) | (i==ni & j==nj)) { # If first (top-left) or last (bottom-right) graph, then remove ticks and labels
        if(side %% 2L == 1L) Axis(x, side = side, xpd = xpd, tick=FALSE, labels=FALSE, ...) # Specifies top & bottom axes
        else Axis(y, side = side, xpd = xpd, tick=FALSE, labels=FALSE, ...) # specifies left & right axes
      } else {
        if(side %% 2L == 1L) Axis(x, side = side, xpd = xpd, ...) # Specifies top & bottom axes
        else Axis(y, side = side, xpd = xpd, ...) # specifies left & right axes
      }
    }
    
    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
    localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
    
    dots <- list(...); nmdots <- names(dots)
    if (!is.matrix(x)) {
      x <- as.data.frame(x)
      for(i in seq_along(names(x))) {
        if(is.factor(x[[i]]) || is.logical(x[[i]]))
          x[[i]] <- as.numeric(x[[i]])
        if(!is.numeric(unclass(x[[i]])))
          stop("non-numeric argument to 'pairs'")
      }
    } else if (!is.numeric(x)) stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
      lower.panel <- match.fun(lower.panel)
    if((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
      upper.panel <- match.fun(upper.panel)
    if((has.diag  <- !is.null( diag.panel)) && !missing( diag.panel))
      diag.panel <- match.fun( diag.panel)
    
    if(row1attop) {
      tmp <- lower.panel; lower.panel <- upper.panel; upper.panel <- tmp
      tmp <- has.lower; has.lower <- has.upper; has.upper <- tmp
    }
    
    nc <- ncol(x)
    if (nc < 2L) stop("only one column in the argument to 'pairs'")
    
    if(!all(1L <= horInd & horInd <= nc))
      stop("invalid argument 'horInd'")
    if(!all(1L <= verInd & verInd <= nc))
      stop("invalid argument 'verInd'")
    
    if(doText) {
      if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels)) labels <- paste("var", 1L:nc)
      }
      else if(is.null(labels)) doText <- FALSE
    }
    oma  <- if("oma"  %in% nmdots) dots$oma
    main <- if("main" %in% nmdots) dots$main
    title_extra=str_count(main,"\n") # carriage returns in the title
    if (is.null(oma))
      oma <- c(if(!is.null(main)) 7 + title_extra*4.5 else 4, if(!is.null(main)) 7 + title_extra*4.5 else 4, if(!is.null(main)) 7 + title_extra*4.5 else 4, if(!is.null(main)) 7 + title_extra*4.5 else 4)
    opar <- par(mfcol = c(length(horInd), length(verInd)), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    dev.hold(); on.exit(dev.flush(), add = TRUE)
    
    xl <- yl <- logical(nc)
    if (is.numeric(log)) xl[log] <- yl[log] <- TRUE
    else {xl[] <- grepl("x", log); yl[] <- grepl("y", log)}
    
    ni <- length(iSet <- if(row1attop) horInd else rev(horInd))
    nj <- length(jSet <- verInd)
    for(j in jSet)
      for(i in iSet) {
        l <- paste0(if(xl[j]) "x" else "", if(yl[i]) "y" else "")
        
        if (is.null(xlim) & is.null(ylim)) localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, type = "n", ..., log = l)
        if (is.null(xlim) & !is.null(ylim)) localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, type = "n", ..., log = l, ylim = ylim[j,i,])
        if (!is.null(xlim) & is.null(ylim)) localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, type = "n", ..., log = l, xlim = xlim[j,i,])
        if (!is.null(xlim) & !is.null(ylim)) localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, type = "n", ..., log = l, xlim = xlim[j,i,], ylim=ylim[j,i,])
        
        if(i == j || (i < j && has.lower) || (i > j && has.upper) ) {
          box()
          j.odd <- (match(j, jSet) + horOdd) %% 2L
          i.odd <- (match(i, iSet) + verOdd) %% 2L
          # if lower panel has data
          if(ticks[1]){
            if(i == iSet[ni] && (j.odd || !j.odd || !has.upper || !has.lower)) {par(mgp = c(3, adj, 0)); localAxis(1L, x[, j], x[, i], ...)} # bottom ... j.odd & !j.odd makes axis appear on all *bottom* graphs and par manages distance of numbers from axis
            if(j == jSet[1L] && (i.odd || !i.odd || !has.upper || !has.lower)) {par(mgp = c(3, 1, 0)); localAxis(2L, x[, j], x[, i], ...)} # left ... i.odd & !i.odd makes axis appear on all *left* graphs and par manages distance of numbers from axis
          }
          # if upper panel has data
          if(ticks[2]){
            if(i == iSet[1L] && (j.odd || !j.odd || !has.upper || !has.lower)) {par(mgp = c(3, 1, 0)); localAxis(3L, x[, j], x[, i], ...)} # top ... j.odd & !j.odd makes axis appear on all *top* graphs and par manages distance of numbers from axis
            if(j == jSet[nj] && (i.odd || !i.odd || !has.upper || !has.lower)) {par(mgp = c(3, adj, 0)); localAxis(4L, x[, j], x[, i], ...)} # right ... i.odd & !i.odd makes axis appear on all *right* graphs and par manages distance of numbers from axis
          }
          
          #if(i == iSet[1L] && (!j.odd || !has.upper || !has.lower)) localAxis(3L, x[, j], x[, i], ...) # top ... used to put axis on a subset of top graphs
          #if(i == iSet[ni] && (j.odd || !j.odd || !has.upper || !has.lower)) {par(mgp = c(3, adj, 0)); localAxis(1L, x[, j], x[, i], ...)} # bottom ... j.odd & !j.odd makes axis appear on all bottom graphs
          #if(j == jSet[1L] && (i.odd || !i.odd || !has.upper || !has.lower)) {par(mgp = c(3, 1, 0)); localAxis(2L, x[, j], x[, i], ...)} # left ... i.odd & !i.odd makes axis appear on all left graphs
          #if(j == jSet[nj] && ( i.odd || !has.upper || !has.lower)) localAxis(4L, x[, j], x[, i], ...) # right ... used to put axis on a subset of right graphs
          
          mfg <- par("mfg")
          if(i == j) {
            if (has.diag) localDiagPanel(as.vector(x[, i]), ...)
            if (doText) {
              par(usr = c(0, 1, 0, 1))                                       # sets the graph dimensions to 0 (left & bottom) & 1 (right & top)
              rect(0, 0, 1, 1, col="#E9E9E9")                                # color the diagonal
              if(is.null(cex.labels)) {                                      # if have no pre-specified size for diagonal text
                l.wid <- strwidth(labels, "user")                            # measure width of all strings and substrings
                cex.labels <- max(.8, min(5, .9 / max(l.wid)))               # find an appropriate size for text based on the above
              }
              xlp <- if(xl[i]) 10^0.5 else 0.5                               # compute x position
              ylp <- if(yl[j]) 10^label.pos else label.pos                   # compute y position
              l=labels[i]                                                    # get current label
              if(substr(l,nchar(l),nchar(l))=="\n") l=paste(l,"\n");         # if carriage return at end, add extra one b/c one gets chopped off by strsplit
              l1=unlist(strsplit(l, split="\n", fixed=TRUE));                # make list of l, split by carriage returns
              len=length(l1);                                                # record length of l1
              l2=l1                                                          # duplicate 1 as l2
              if (len>1) l1[2:len]=""                                        # remove any non-first item(s) from l1
              l1=paste0(l1,collapse="\n")                                    # paste l1 back together
              text.panel(xlp, ylp, l1, cex=cex.labels, font=2)               # print l1
              l2[1]=""                                                       # remove first item from l2
              l2=paste0(l2,collapse="\n")                                    # paste l2 back together
              text.panel(xlp, ylp, l2, cex=cex.labels, font=font.labels)     # print l2
            }
          } 
          else if(i < j) localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), as.vector(jdata[, j]), as.vector(jdata[, i]), sp, nvar, lw, ss, smoothness, digits, cohensd, perc_rank, showp, domedian, domean, do95CI, dotint, panelcolor, tintmaxdiff, ...)
          else localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), as.vector(jdata[, j]), as.vector(jdata[, i]), sp, nvar, lw, ss, smoothness, digits, cohensd, perc_rank, showp, domedian, domean, do95CI, dotint, panelcolor, tintmaxdiff, ...)
          if (any(par("mfg") != mfg))
            stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
      }
    if (!is.null(main)) {
      font.main <- if("font.main" %in% nmdots) dots$font.main else par("font.main")
      cex.main  <- if("cex.main"  %in% nmdots) dots$cex.main  else par("cex.main")
      mtext(main, 3, line.main, outer=TRUE, at = 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
  }