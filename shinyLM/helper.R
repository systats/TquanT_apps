fit_plane <- function(data){
  
  fit_plane <- lm(y ~ x1 + x2, data = data)
  
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

plot_plane <- function(data, surface, variance){

  graph_reso <- 0.05
  axis_x1 <- seq(0, 10, by = graph_reso)
  axis_x2 <- seq(0, 10, by = graph_reso)
  
  library(plotly)
  data$id <- seq_len(nrow(data))
  
  ms <- replicate(2, data, simplify = F)
  ms[[2]]$y <- lm(y ~ x1 + x2, ms[[2]]) %>%
    predict(., ms[[2]])
  m <- plotly::group2NA(dplyr::bind_rows(ms), "id")
  
  ms1 <- replicate(2, data, simplify = F)
  ms1[[1]]$y <- mean(ms1[[1]]$y)
  ms1[[2]]$y <- lm(y ~ x1 + x2, ms1[[2]]) %>%
    predict(., ms[[2]])
  m1 <- plotly::group2NA(dplyr::bind_rows(ms1), "id")
  # p <- plotly::plot_ly(showlegend = F) %>%
  #   add_markers(data = mtcars, x = ~disp, y = ~wt, z = ~mpg) %>%
  #   
  p <- plotly::plot_ly(
    data,
    x = ~ x1,
    y = ~ x2,
    z = ~ y,
    type = "scatter3d",
    mode = "markers", 
    height = 700,
    width = 700,
    showlegend = F,
    color = I("black")
  ) %>%
  plotly::add_trace(
    p = .,
    z = surface,
    x = axis_x1,
    y = axis_x2,
    type = "surface", 
    showlegend = F
  ) 
  
  if("resid" %in% variance){
    p <- p %>%
      add_paths(
        data = m,
        x = ~ x1,
        y = ~ x2,
        z = ~ y, 
        color = I("red")
      )
  }
  
  if("model" %in% variance){
    
    p <- p %>%
      add_paths(
        data = m1,
        x = ~ x1,
        y = ~ x2,
        z = ~ y, 
        color = I("green")
      )
  }
  
  return(p)
}