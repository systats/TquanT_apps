fit_plane <- function(data){
  
  fit_plane <- lm(y ~ 1 + x1 + x2, data = data)
  
  #Graph Resolution (more important for more complex shapes)
  graph_reso <- 0.05
  
  #Setup Axis
  axis_x1 <- seq(0, 10, by = graph_reso)
  axis_x2 <- seq(0, 10, by = graph_reso)
  
  #Sample points
  fit_plane_surface <- expand.grid(
    x1 = axis_x1,
    x2 = axis_x2,
    KEEP.OUT.ATTRS = F
  )
  
  fit_plane_surface$y <- predict.lm(fit_plane, newdata = fit_plane_surface)
  fit_plane_surface <- reshape2::acast(fit_plane_surface, x2 ~ x1, value.var = "y")
  
  return(fit_plane_surface)
}

plot_plane <- function(data, surface){

  graph_reso <- 0.05
  axis_x1 <- seq(0, 10, by = graph_reso)
  axis_x2 <- seq(0, 10, by = graph_reso)
  
  p <- plotly::plot_ly(
    data,
    x = ~ x1,
    y = ~ x2,
    z = ~ y,
    type = "scatter3d",
    mode = "markers", 
    height = 700,
    width = 700
  ) %>%
  plotly::add_trace(
    p = .,
    z = surface,
    x = axis_x1,
    y = axis_x2,
    type = "surface", 
    showlegend = F
  ) %>%
  plotly::layout(showlegend = F) 
  
  return(p)
}