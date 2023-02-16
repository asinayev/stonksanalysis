
plot_stock=function(stockdat,symb, target_date, before=10, after=20){
  target_date=as.Date(target_date)
  x=stockdat[symbol==symb & date %between% c(target_date-before, target_date+after)]
  x%>%
  ggplot(aes(x = date, y = close)) +
    geom_vline(xintercept = target_date, linetype="dashed", size=0.5) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    # labs(title = "AAPL Bar Chart", 
    #      subtitle = "Zoomed in using coord_x_date",
    #      y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(target_date-before, target_date+after),
                 ylim = c(min(x$low), max(x$high))) +
    theme_tq() 
}

