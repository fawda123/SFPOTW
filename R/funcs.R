# plotting function
# raw data or summarized
plo_fun <- function(seldat, sumsel, yearsel, barsel = F, colsin = NULL){

  req(seldat)

  if(!barsel){

    seldat <- summarise(seldat,
                     mag = sum(mag, na.rm = T),
                     .by = c('month', 'year', 'date', 'param'))
  }

  # raw data
  if(!sumsel){

    # x axis ranges
    xrng <- range(seldat$date) + c(-15, 15)

    if(!barsel){

      # yaxis range
      ymax <- max(seldat$mag, na.rm = T)

      # plot
      p <- plotly::plot_ly(seldat, x = ~date, y = ~mag, type = 'scatter', mode = 'lines+markers',
                           marker = list(color = 'rgb(0, 174, 239)'),
                           line = list(color = 'rgb(0, 174, 239)'))

    }

    if(barsel){

      # axis ranges
      ymax <- NA

      # plot
      p <- plotly::plot_ly(seldat, x = ~date, y = ~mag, type = 'bar', color = ~POTW, colors = colsin)

    }

  }

  # summarized data
  if(sumsel){

    req(yearsel)

    validate(
      need(length(unique(yearsel)) == 2, 'At least two months needed to summarize')
    )

    # month filter as numeric
    mos <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
    tofilt <- factor(yearsel, levels = mos, labels = c(1:12)) %>%
      as.numeric()

    # xaxis range
    xrng <- range(seldat$year)

    # summarize data
    seldat <- seldat %>%
      filter(month >= tofilt[1] & month <= tofilt[2])

    # remove start and end years if doesn't contain number of months in tofilt
    torm <- seldat %>%
      summarise(
        n = n_distinct(month),
        .by = year
      )
    minmo <- length(seq(tofilt[1], tofilt[2]))
    seldat <- seldat %>%
      filter(!year %in% torm$year[which(torm$n != minmo)])

    if(!barsel){

      seldat <- seldat %>%
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

    if(barsel){

      seldat <- seldat %>%
        summarise(
          mag = mean(mag, na.rm = T),
          .by = c('year', 'POTW')
        )

      # ymax
      ymax <- NA
      xrng <- xrng + c(-0.5, 0.5)

      # plot
      p <- plotly::plot_ly(seldat, x = ~year, y = ~mag, type = 'bar', color = ~POTW, colors = colsin)

    }

  }

  p <- p %>%
    plotly::layout(
      xaxis = list(title = NA, range = xrng),
      yaxis = list(title = 'Magnitude', range = c(0, ymax)),
      showlegend = F,
      margin = list(b = 0),
      barmode = 'stack'
    )

  # print
  return(p)

}
