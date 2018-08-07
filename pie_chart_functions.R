createCatPieCharts <- function(pie_data1, pie_data2, left_title, right_title){
  p1 <- ggplotly(intact.cat.pie(pie_data1)) %>% plotly_data()
  p2 <- ggplotly(intact.cat.pie(pie_data2)) %>% plotly_data()
  pp <- plot_ly() %>%
    add_pie(data = p1, labels = ~tag, values = ~Percentage, 
            textposition = 'inside',
            textinfo = 'label',
            domain = list(x = c(0, 0.45), y = c(0, 1)),
            marker = list(colors = p1$Color)) %>%
    add_pie(data = p2, labels = ~tag, values = ~Percentage, 
            textposition = 'inside',
            textinfo = 'label',
            domain = list(x = c(0.55, 1), y = c(0, 1)),
            marker = list(colors = p2$Color)) %>%
    layout(showlegend = FALSE, annotations = list(
      list(
        x = 0.225, 
        y = 1.0, 
        font = list(size = 16), 
        showarrow = FALSE, 
        text = left_title, 
        xanchor = "center", 
        xref = "paper", 
        yanchor = "bottom", 
        yref = "paper"
      ),
      list(
        x = 0.775, 
        y = 1.0, 
        font = list(size = 16), 
        showarrow = FALSE, 
        text = right_title, 
        xanchor = "center", 
        xref = "paper", 
        yanchor = "bottom", 
        yref = "paper"
      )
    ))
  return(pp)
}
createMainPieCharts <- function(pie_data1, pie_data2, left_title, right_title){
  p1 <- ggplotly(intact.main.pie(pie_data1)) %>% plotly_data()
  p2 <- ggplotly(intact.main.pie(pie_data2)) %>% plotly_data()
  pp <- plot_ly() %>%
    add_pie(data = p1, labels = ~tag, values = ~Percentage, 
            textposition = 'inside',
            textinfo = 'label',
            domain = list(x = c(0, 0.45), y = c(0, 1)),
            marker = list(colors = p1$Color)) %>%
    add_pie(data = p2, labels = ~tag, values = ~Percentage, 
            textposition = 'inside',
            textinfo = 'label',
            domain = list(x = c(0.55, 1), y = c(0, 1)),
            marker = list(colors = p2$Color)) %>%
    layout(showlegend = FALSE, annotations = list(
      list(
        x = 0.225, 
        y = 1.0, 
        font = list(size = 16), 
        showarrow = FALSE, 
        text = left_title, 
        xanchor = "center", 
        xref = "paper", 
        yanchor = "bottom", 
        yref = "paper"
      ),
      list(
        x = 0.775, 
        y = 1.0, 
        font = list(size = 16), 
        showarrow = FALSE, 
        text = right_title, 
        xanchor = "center", 
        xref = "paper", 
        yanchor = "bottom", 
        yref = "paper"
      )
    ))
  return(pp)
}
createSubPieCharts <- function(pie_data1, pie_data2, left_title, right_title){
  p1 <- ggplotly(intact.sub.pie(pie_data1)) %>% plotly_data()
  p2 <- ggplotly(intact.sub.pie(pie_data2)) %>% plotly_data()
  pp <- plot_ly() %>%
    add_pie(data = p1, labels = ~tag, values = ~Percentage, 
            textposition = 'inside',
            textinfo = 'label',
            domain = list(x = c(0, 0.45), y = c(0, 1)),
            marker = list(colors = p1$Color)) %>%
    add_pie(data = p2, labels = ~tag, values = ~Percentage, 
            textposition = 'inside',
            textinfo = 'label',
            domain = list(x = c(0.55, 1), y = c(0, 1)),
            marker = list(colors = p2$Color)) %>%
    layout(showlegend = FALSE, annotations = list(
      list(
        x = 0.225, 
        y = 1.0, 
        font = list(size = 16), 
        showarrow = FALSE, 
        text = left_title, 
        xanchor = "center", 
        xref = "paper", 
        yanchor = "bottom", 
        yref = "paper"
      ),
      list(
        x = 0.775, 
        y = 1.0, 
        font = list(size = 16), 
        showarrow = FALSE, 
        text = right_title, 
        xanchor = "center", 
        xref = "paper", 
        yanchor = "bottom", 
        yref = "paper"
      )
    ))
  return(pp)
}