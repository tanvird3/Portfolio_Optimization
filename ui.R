library(shiny)
shinyUI(fluidPage (
  theme = shinytheme("darkly"),
  headerPanel("Portfolio Optimization"),
  sidebarPanel(
    width = 3,
    selectInput(
      "stockX",
      label = "Select Instruments",
      choices =
        tt,
      selected = c(tt[which(tt == "ACI")], tt[which(tt == "BRACBANK")], tt[which(tt ==
                                                                                   "SQURPHARMA")], tt[which(tt == "GP")], tt[which(tt == "DBH")]),
      multiple = T
    ),
    textInput("startdate", label = "Start Date (yyyy-mm-dd)", value =
                (Sys.Date() - 180)),
    textInput("enddate", label = "End Date (yyyy-mm-dd)", value =
                Sys.Date()),
    numericInput(
      'minpct',
      label = "Min % to be Allocated to an Inst.",
      value =
        5,
      min = 0,
      max = 100
    ),
    numericInput(
      'maxpct',
      label = "Max % to be Allocated to an Inst.",
      value =
        80,
      min = 0,
      max = 100
    ),
    selectInput(
      "optimize",
      label = "Select Optimization Method",
      choices = c("Maximize Retrun", "Minimize Shortfall"),
      selected = "Minimize Shortfall"
    ),
    submitButton(text = "Optimize")
  ),
  mainPanel (h1(""),
             tabsetPanel(
               tabPanel("Optimum Weight", plotlyOutput("p")),
               tabPanel("VaR & CVaR", plotlyOutput("Y")),
               tabPanel("Expected Return", plotlyOutput("Z"))
               #tabPanel("Summary Output", verbatimTextOutput("X"))
             ))
))
