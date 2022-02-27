# multiple withins
library(shiny)
library(psychometric)
library(stringr)
library(tidyr)
#library(dplyr)
source("functions/equate_raw_axis_ranges.R")    # Need
source("functions/pairs2wn.R")                  # Need
source("functions/panel_cor2wn.R")              # Need
source("functions/points_and_lines_wn.R")       # Need
source("functions/perc_rank_rm.R")              # Need
source("functions/isolate_complete_pairs.R")    # Redundant with multiplecorrelations
source("functions/jitter_by_percent_min_wn.R")  # Need


shinyServer( 
  function(input, output) { # Create the function
    # Grabs user-specified height and width
    output$ui_plot <- renderUI({plotOutput("contents", width = input$plotsize*8, height = input$plotsize*8)})
    output$contents <- renderPlot( { # Call Shiny function that makes the plot
    # Process any pasted data
      if(input$myData>"") {
        t <- read.table(text = input$myData, sep = '\t', header = TRUE); 
        t=lapply(t, as.numeric); t=as.data.frame(t);
        tt <- read.table(text = input$myData, sep = '\t', header = FALSE); 
        tt=lapply(tt, as.numeric); tt=as.data.frame(tt);
        # The entire top row should be NA if they've entered variable names ... 
        # if so, assume header, if not, don't assume header
        if (all(is.na(tt[1,]))) t=t else t=tt
      } else {
        t=read.csv("within_demodata.csv",check.names=FALSE)
      } 
      v=colnames(t); v=gsub("\\.", " ", as.character(v))
    # Transform data into percentile ranks
    if (input$spearman==TRUE) t=perc_rank_rm(t) 
    # Figure out max difference
    nc=ncol(t) # find number of columns
    diffs=array(0,c(nc,nc));
    for (i in 1:ncol(t)) { # for each column of graphs
      for (j in 1:ncol(t)) { # for each graph in this column of graphs
        diffs[i,j]=mean(t[,i], na.rm = TRUE)-mean(t[,j], na.rm = TRUE)
      }
    }
    1==1
    maxtintdiff = max(abs(diffs), na.rm = TRUE)
    # Jitter data percent of minimum difference between points for each column
    t1=jitter_by_percent_min_wn(t,input$jitter_perc)
    t1=t;
    # Get variable labels
    if(input$variablelabels=="") colnames(t)=v 
    else {
      rng=input$variablelabels
      rng=unlist(strsplit(rng,","))
      if (length(rng) < length(v)) {
        rng2=rng
        rng2[(length(rng)+1):length(v)]=v[(length(rng)+1):length(v)]
      } 
      else {
        rng2=rng[1:length(v)]
      }
      colnames(t)=rng2 
    }
# Choose axis ranges
    ranges=equate_raw_axis_ranges(t, cushion=.1) # Could in very rare cases be cutting off data points
    nc=ncol(t) # find number of columns
    xlim=array(0,c(nc,nc,2)); ylim=array(0,c(nc,nc,2));
    for (i in 1:ncol(t)) { # for each column of graphs
      for (j in 1:ncol(t)) { # for each graph in this column of graphs
        xlim[j,i,]=ranges[j,] # set the x range
        ylim[j,i,]=ranges[i,] # set the y range
      }
    }
# Draw the scatterplot
    if(input$tint==FALSE) {tintcolor=rgb(1,1,1); dotint=0} else {tintcolor=input$color1; dotint=1}
    if(input$fitline==TRUE & input$fitcurve==FALSE) p="my_line_rm" else if (input$fitline==FALSE & input$fitcurve==TRUE) p="my_curve_rm" else if (input$fitline==TRUE & input$fitcurve==TRUE) p="my_lineandcurve_rm" else if (input$fitline==FALSE & input$fitcurve==FALSE) p="my_points_rm"
    if(input$upper=="stats") u="panel_cor2wn" else if (input$upper=="data") u=p else if (input$upper=="neither") u=NULL
    if(input$lower=="stats") l="panel_cor2wn" else if (input$lower=="data") l=p else if (input$lower=="neither") l=NULL
    ticks=c(FALSE,FALSE); if(input$lower=="data") ticks[1]=TRUE; if(input$upper=="data") ticks[2]=TRUE; 
    adj=(input$ticklabelsize+10)/39 # a hack to adjust the bottom tick labels to match the left tick labels as they get bigger & smaller
    makemyplot <- function() {
      par(pty="s")
      pairs2wn(t, panel=p, cex.axis=(input$ticklabelsize+1)/25, adj=adj, 
             upper.panel=u, lower.panel=l, xlim=xlim, ylim=ylim, jdata=t1, 
             pch=as.numeric(input$dottype), cex=input$dotsize/20, 
             col=rgb(red=0.0, green=0.0, blue=0.0, alpha=input$dotopacity/100),
             main=input$graphtitle, cex.main=3, lw=input$lw/10, ss=input$ss/20, smoothness=input$smoothness/100,
             digits=input$digits, perc_rank=input$spearman, sp=FALSE, domedian=input$addmedian, domean=input$addmean, do95CI=input$add95ci,
             cohensd=input$standardizestats, ticks=ticks,
             dotint=dotint, panelcolor=tintcolor, tintmaxdiff=maxtintdiff)
    }
    makemyplot()

  # Save as 'filename' the 'content'
    output$down <- downloadHandler(
      filename =  function() {
        paste("myplot", input$filetype, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$filetype == "png")
          png(file, units="in", width=input$plotsize/10, height=input$plotsize/10, res=500) # make png file
        else
          pdf(file, width=input$plotsize/10, height=input$plotsize/10) # open the pdf device
        makemyplot()
        dev.off()  # turn the device off
      })
  })
  })