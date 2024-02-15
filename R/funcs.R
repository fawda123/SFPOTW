# plotting function
# raw data or summarized
plo_fun <- function(seldat, sumsel, yearsel){

  req(seldat)

  # raw data
  if(!sumsel){

    # xaxis range
    xrng <- range(seldat$date)
    ymax <- max(seldat$mag, na.rm = T)

    # plot
    p <- plotly::plot_ly(seldat, x = ~date, y = ~mag, type = 'scatter', mode = 'lines+markers',
                         marker = list(color = 'rgb(0, 174, 239)'),
                         line = list(color = 'rgb(0, 174, 239)'))
  }

  # summarized data
  if(sumsel){

    req(yearsel)

    validate(
      need(length(unique(yearsel)) == 2, 'At least two months needed to summarize')
    )

    # month filter as numeric
    tofilt <- factor(yearsel, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), labels = c(1:12)) %>%
      as.numeric()

    # xaxis range
    xrng <- range(seldat$year)

    # summarize data
    seldat <- seldat %>%
      filter(month >= tofilt[1] & month <= tofilt[2]) %>%
      summarise(
        ave = mean(mag, na.rm = T),
        lov = tryCatch(t.test(mag, na.rm = T)$conf.int[1], error = function(err) NA),
        hiv = tryCatch(t.test(mag, na.rm = T)$conf.int[2], error = function(err) NA),
        .by = year
      ) %>%
      mutate(
        ave = ifelse(is.na(lov), NA, ave)
      )

    # ymax
    ymax <- max(seldat$hiv, na.rm = T)

    # plot
    p <- plotly::plot_ly(seldat, x = ~year, y = ~ave, type = 'scatter', mode = 'lines',
                         line = list(color = 'rgb(0, 174, 239)')) %>%
      plotly::add_ribbons(ymin = ~lov,  ymax = ~hiv, fillcolor = 'rgba(0, 174, 239, 0.2)', line = list(color = 'rgba(0, 174, 239, 0)'))

  }

  p <- p %>%
    plotly::layout(
      xaxis = list(title = NA, range = xrng),
      yaxis = list(title = 'Magnitude', range = c(0, ymax)),
      showlegend = F
    )

  # print
  return(p)

}
