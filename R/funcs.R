plo_fun <- function(seldat, sumsel, yearsel){

  req(seldat)

  # raw data
  if(!sumsel){

    # xaxis range
    xrng <- range(seldat$date)

    # plot
    p <- plotly::plot_ly(seldat, x = ~date, y = ~mag, type = 'scatter', mode = 'lines+markers',
                         marker = list(color = 'rgb(0, 174, 239)'),
                         line = list(color = 'rgb(0, 174, 239)'))
  }

  # summarized data
  if(sumsel){

    req(yearsel)

    # summarize data
    tofilt <- as.numeric(format(yearsel, '%m'))
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

    # xaxis range
    xrng <- range(seldat$year)

    # plot
    p <- plotly::plot_ly(seldat, x = ~year, y = ~ave, type = 'scatter', mode = 'lines',
                         line = list(color = 'rgb(0, 174, 239)')) %>%
      plotly::add_ribbons(ymin = ~lov,  ymax = ~hiv, fillcolor = 'rgba(0, 174, 239, 0.2)', line = list(color = 'rgba(0, 174, 239, 0)'))

  }

  p <- p %>%
    plotly::layout(
      xaxis = list(title = NA, range = xrng),
      yaxis = list(title = 'Magnitude'),
      showlegend = F
    )

  # print
  return(p)

}
