# multiple withins
library(shiny)
library(colourpicker)
library("rclipboard") # added for URL project 3/22/24

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  rclipboardSetup(), # added for URL project 3/22/24
  
  # Application title
  titlePanel("Repeated measures: multiple measures"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
    
      # Data input
      textAreaInput("myData", "DATA", "", width = 200, height = 200, placeholder = "[Paste, from spreadsheet, 2+ columns of data with non-number labels in top row]"),
      
      # Hacks
      tags$style("input[type='checkbox']:checked+span{font-weight:bold;}"), # hack to get checkboxes to show up bold when unchecked
      tags$style("input[type='checkbox']:not(:checked)+span{font-weight:bold;}"), # hack to get checkboxes to show up bold when unchecked
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # hack to remove minor ticks on sliders
      
      # Options to select from
      selectInput(inputId="options", label="OPTIONS:",
                  choices=c("*** select ***" = "select",
                            "Manage data point visibility" = "dotvisibility",
                            "Transform data" = "perc",
                            "Show stats/data/colors" = "subplotcontents",
                            "Display lines" = "fit",
                            "Manage mean/median/stats" = "stats",
                            "Adjust labels & plot size" = "labels",
                            "Data import" = "dataimport"),
                  selected = NULL),
      
      # Transform data
      conditionalPanel(condition="input.options=='perc'",
                       checkboxInput('spearman', 'percentile ranks', FALSE)
      ),
      
      # Stats
      conditionalPanel(condition="input.options=='stats'",
                                        checkboxInput('addmean', 'mean', TRUE),
                                        checkboxInput('add95ci', 'margin of error (95% CI)', TRUE),
                                        checkboxInput('addmedian', 'median', FALSE),
                                        sliderInput(inputId = "ss",
                                                    label = "symbol size",
                                                    min = 0,
                                                    max = 100,
                                                    value = 50),
                                        checkboxInput('standardizestats', 'standardize mean difference', FALSE),
                                        sliderInput(inputId = "digits",
                                                    label = "digits",
                                                    min = 2,
                                                    max = 5,
                                                    value = 2)
                       ),

      conditionalPanel(condition="input.options=='subplotcontents'",
                                        radioButtons("upper", label = "upper panel", choices = list("stats", "data", "neither"), selected="data"),
                                        radioButtons("lower", label = "lower panel", choices = list("stats", "data", "neither"), selected="neither"),

                                        checkboxInput('tint', 'color by size of difference', FALSE),
                                        conditionalPanel(condition="input.tint",
                                        colourInput(inputId="color1", label=NULL, value = "purple", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE)
                       ),
                       conditionalPanel(condition="input.upper=='stats' | input.lower=='stats'",
                                        checkboxInput('showp', 'show p in place of n', FALSE)     
                       )
                       ),

      conditionalPanel(condition="input.options=='fit'",
      sliderInput(inputId = "lw",
                  label = "line widths",
                  min = 0,
                  max = 100,
                  value = 25),
      checkboxInput('fitline', 'fit line', FALSE),
      checkboxInput('fitcurve', 'fit curve', FALSE),
      sliderInput(inputId = "smoothness",
                  label = "curve smoothness",
                  min = 0,
                  max = 100,
                  value = 50)
      ),
      
      
      conditionalPanel(condition="input.options=='dotvisibility'",
                       radioButtons("dottype", label = "type", choiceNames = list("ring", "dot"), choiceValues = list("1","16")),
                       sliderInput(inputId = "dotsize",
                                   label = "size",
                                   min = 1,
                                   max = 100,
                                   value = 30),
                       sliderInput(inputId = "jitter_perc",
                                   label = "jitter",
                                   min = 0,
                                   max = 100,
                                   value = 0),
                       sliderInput(inputId = "dotopacity",
                                   label = "opacity",
                                   min = 0,
                                   max = 100,
                                   value = 100)
      ),

      
      conditionalPanel(condition="input.options=='labels'",
                       textAreaInput("graphtitle", label = "title", value = "", width = "100%", placeholder = "Use [return] to split title"),
                       textAreaInput("variablelabels", label = "variable labels", value = "", width = "100%", rows = "2", placeholder = "v1,v2,v3... use [return] to split labels"),
                       sliderInput(inputId = "ticklabelsize",
                                   label = "axis number size",
                                   min = 0,
                                   max = 100,
                                   value = 30),
                       sliderInput(inputId = "plotsize",
                                   label = "plot size",
                                   min = 0,
                                   max = 200,
                                   value = 100)
                       ),
      
      # Data import
      conditionalPanel(condition="input.options=='dataimport'",
                       textInput("datalink", 
                                 label = HTML("paste shared google sheets link<h6><strong style='font-weight:normal'>
                                 Linked file must contain <i>only</i> the data you wish to plot, with a top row of column labels and 2+ columns of numbers. Column labels must be text, not numbers.</strong></h6>"), 
                                 value = "", width = "85%", placeholder = "https://docs.google.com/spread...")
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput('ui_plot'),
      tags$h6(HTML(" ")),
      downloadButton(outputId = "down", label = "Download graph as..."),
      radioButtons("filetype", label = NULL, choices = list("png", "pdf")),
      
      # added for URL project 3/22/24
      uiOutput("clip"), 
      tags$h6(HTML(" ")),

      hr(style = "margin: 0px 30px 20px 30px; border: .5px solid #a6a6a6"),
      tags$h6("Notes..."),
      tags$h6("1. Confidence interval (CI): 95% CI is computed, as in a paired t-test, on the distances from the x=y line."),
      tags$h6("2. Lines: default is x=y line, with options for least-squares and curved lines."),
      tags$h6("3. Curve: spline via R's smooth.spline function, with smoothness set via 'spar' argument."),
      tags$h6("4. Jitter: for raw data, unit is percentage of smallest distance between two dots, calculated separately for each variable."),
      tags$h6("5. Standardize: transforms statistics from original units to standard deviation (Cohen's d) units."),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }")
    )
  )
))