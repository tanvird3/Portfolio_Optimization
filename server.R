shinyServer(function(input, output) {
  ff <- function(stockX,
                 date1,
                 date2,
                 optimize,
                 minpct,
                 maxpct) {
    # from the split list of data frames(according to the instruments) extract only instrument name and closing
    # price and put them in column under the instrument's name
    reticulate::source_python("dse.py")
    
    d <-
      dse_hist(date1,
               date2,
               stockX)
    
    dd <- split(d, f = d$TRADING.CODE)
    
    wo <- lapply(dd, function(x)
      as.data.frame(x[, c(1, 7)]))
    
    # join the list of data frames into one single data frame, where the columns are
    # DATE and Istrument Names (the values are the closing prices)
    ddi <- join_all(wo, by = "DATE", type = "left")
    
    # name the columns according to the instrument names
    names(ddi) <- c("DATE", names(wo))
    
    ddi <- mutate(ddi, DATE = as.Date(DATE, format = "%Y-%m-%d"))
    
    ddi[is.na(ddi)] <- 0
    names(ddi) <- gsub("^\\s+|\\s+$", "", names(ddi))
    ddi[is.na(ddi)] <- 0
    
    # remove duplicates if it exists
    ww <- which(duplicated(ddi$Date) == TRUE)
    if (length(ww > 0)) {
      ddi <- ddi[-ww, ]
    }
    
    # select DATE column and only the selected insturments from the menu
    ddi <- select(ddi, DATE, all_of(stockX))
    nn <- names(ddi)[-1]
    
    # filter data (from the menu)
    ddi <-
      dplyr::filter(
        ddi,
        DATE >= as.Date(date1, format = "%Y-%m-%d") &
          DATE <= as.Date(date2, format = "%Y-%m-%d")
      )
    
    # calculate the return series
    returns <- ddi
    R <- xts(returns[, -1], order.by = returns$DATE)
    R <- Return.calculate(R)
    R[is.na(R)] <- 0
    R[!is.finite(R)] <- 0
    
    # get the fund names
    fund.names <- colnames(ddi)[-1]
    
    # rename the return data frame as per the insturment names
    names(R) <- fund.names
    
    # set the portfolio
    pspec <- portfolio.spec(assets = fund.names)
    pspec <-
      add.constraint(portfolio = pspec, type = "full_investment") # 100% will be invested
    pspec <-
      add.constraint(portfolio = pspec, type = "long_only") # no short trading
    pspec <-
      add.constraint(
        portfolio = pspec,
        type = "box",
        min = minpct / 100,
        max = maxpct / 100
      ) # minimum weight given on any insturment is .05 and maximum .80, if required the mimimum might increase
    # but maximize wont increase unless only one insturment is selected
    
    if (optimize == "Minimize Shortfall") {
      pspec <-
        add.objective(
          portfolio = pspec,
          type = "risk",
          name = "ES",
          arguments = list(p = .95)
        ) # for minimize shortfall minimize Expected Shortfall or CVaR
    } else {
      pspec <-
        add.objective(portfolio = pspec,
                      type = "return",
                      name = "mean") # maximize mean return here
    }
    okk <-
      optimize.portfolio(
        R = R,
        portfolio = pspec,
        optimize_method = "ROI",
        trace = TRUE
      )
    weight <- okk$weights
    
    # calculate mean return of the portfolio and mu for the VaR and ES calculation
    
    mr <- apply(R, 2 , mean)
    
    mrr <- sum(mr * weight)
    
    mrr <- c(mrr, mrr * 5, mrr * 20) * 100
    
    # VaR & CVaR calculation based on the optimum weight
    v <- VaR(
      R,
      p = .95,
      method = "gaussian",
      portfolio_method = "component",
      invert = F,
      weights = weight
    )
    day <- c(5, 20)
    p1 <- c(v$VaR) * sqrt(day)
    p1 <- round(c(v$VaR, p1), 5) * 100 # VaR for 1, 7 and 30 days
    
    cv <-
      ES(
        R,
        p = .95,
        method = "gaussian",
        portfolio_method = "component",
        invert = F,
        weights = weight
      )
    p2 <- c(cv$ES) * sqrt(day)
    p2 <- round(c(cv$ES, p2), 5) * 100
    
    # data frame for VaR and CVaR and expected return for bar plot
    pl <- data.frame(
      Days = c(1, 5, 20),
      VaR = -p1,
      CVaR = -p2,
      Gain = mrr
    )
    
    
    # this data frame contains intrument names, their respective last day closing price (not of any use here)
    # and optimum weight
    ww <- as.data.frame(weight)
    
    wp <-
      data.frame(
        Instrument_Name = row.names(ww),
        CLOSING_PRICE = ddi[1, (2:ncol(ddi))],
        Weight = ww[, 1],
        row.names = NULL
      )
    
    # pie plot for showing the weights
    p <-
      plot_ly(
        wp,
        labels =  ~ Instrument_Name,
        values =  ~ Weight,
        type = "pie",
        name = "Optimum Weight",
        width = 800,
        height = 500,
        #name = "",
        hovertemplate = "%{label}: %{percent}"
      ) %>%
      layout(
        title = "",
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE, 
          hoverformat = ".2f"
        ),
        showlegend = T,
        autosize = F
      )
    
    # Bar plot for showing VaR and CVaR for the optimum Portfolio
    pt <-
      plot_ly(
        pl,
        x =  ~ as.factor(Days),
        y =  ~ VaR,
        type = "bar",
        name = "Value at Risk",
        marker = list(color = "#009E73"),
        width = 900
      ) %>%
      add_trace(
        y =  ~ CVaR,
        name = "Conditional Value at Risk",
        marker = list(color = "#56B4E9")
      ) %>%
      layout(
        yaxis = list(title = "LOSS (in %)", hoverformat = ".4f"),
        barmode = "group",
        xaxis = list(title = "Days")
      )
    
    # Expected return plot
    ptt <- plot_ly(
      pl,
      x =  ~ as.factor(Days),
      y =  ~ Gain,
      type = "bar",
      name = "Expected Return",
      marker = list(color = "#56B4E9"),
      width = 900
    ) %>%
      layout(
        yaxis = list(title = "Gain / LOSS (in %)", hoverformat = ".4f"),
        barmode = "group",
        xaxis = list(title = "Days")
      )
    return(list(
      pnm = okk,
      p = p,
      pt = pt,
      ptt = ptt
    ))
  }
  #output$X <-
  #renderPrint({
  #ff(input$stockX,
  #input$startdate,
  #input$enddate,
  #input$optimize)$pnm
  #})
  output$p <-
    renderPlotly({
      ff(
        input$stockX,
        input$startdate,
        input$enddate,
        input$optimize,
        input$minpct,
        input$maxpct
      )$p
    })
  output$Y <- renderPlotly({
    ff(
      input$stockX,
      input$startdate,
      input$enddate,
      input$optimize,
      input$minpct,
      input$maxpct
    )$pt
  })
  output$Z <- renderPlotly({
    ff(
      input$stockX,
      input$startdate,
      input$enddate,
      input$optimize,
      input$minpct,
      input$maxpct
    )$ptt
  })
  
  lapply(c("p",
           "Y",
           "Z"), function(x)
             outputOptions(output, x, suspendWhenHidden = F))
})
