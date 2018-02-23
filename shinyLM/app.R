# devtools::install_github("Appsilon/semantic.dashboard")
# devtools::install_github("rstudio/shiny")

library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinyWidgets)
library(dplyr)
library(sjPlot)
library(flexdashboard)
library(scatterD3)

my_dashboard_header <- function(...){
  # verify_value_allowed("color", ALLOWED_COLORS)
  # 
  # inverted_value <- get_inverted_class(inverted)
  shiny::div(class = paste("ui top attached inverted menu"),
    div(class="item", 
      "ShinyLM"
      # img(
      #   src="/images/logo.png", 
      #   style = "width: 100px; margin: 0px 0px; padding: 0px 0px;"
      # )
    ),
    shiny::tags$a(id = "toggle_menu", class = "item", shiny::tags$i(class = "sidebar icon"), "Menu"),
    shiny::div(class = "right icon menu", ...)
  )
}

ui <- dashboardPage(
  my_dashboard_header(color = ""),
  dashboardSidebar(
    side = "left", 
    size = "", 
    inverted = T,
    color = "",
    sidebarMenu(
      menuItem(
        tabName = "sim_lin_reg", 
        text = "Simple Linear Regression"
        #icon = icon("chart")
      ),
      menuItem(
        tabName = "multi_lin_reg", 
        text = "Multivariate Linear Regression"
        #icon = icon("chart")
      )
    )
  ),
  #dashboardBody(
  shiny::div(
    class = "pusher",
    style="min-height: 100vh;",
    tabItems(
      selected = 1,
      tabItem(
        tabName = "sim_lin_reg",
        column(
          width = 11,
          box(
            title = "Sample box", 
            color = "blue", 
            title_side = "top", 
            ribbon = F,
            collapsible = F, 
            width = 11,
            scatterD3::scatterD3Output("plot", width = "100%")
          )
        ),
        column(
          width = 5,
          box(
            title = "Parameter", 
            color = "red", 
            title_side = "top", 
            ribbon = F,
            collapsible = T, 
            width = 5,
            sliderInput("intercept", label = "Intercept", min = -10, max = 10, value = 0, step = 1, animate = T),
            sliderInput("slope", label = "Slope", min = -2, max = 2, value = 1, step = .1, animate = T),
            sliderInput("error", label = "RSE", min = 0, max = 10, value = 2, step = .1, animate = T),
            sliderInput("n", label = "N", min = 10, max = 1000, step = 20, value = 100, animate = T)
          ),
          box(
            title = "Estimation", 
            color = "orange", 
            title_side = "top", 
            ribbon = F,
            collapsible = T, 
            width = 4,
            uiOutput("lm1"),   
            flexdashboard::gaugeOutput("box")
          )
        )
      ),
      tabItem(
        tabName = "multi_lin_reg",
        ""
      )
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    set.seed(2017)
    x <- runif(input$n, 1, 7)
    error <- rnorm(input$n, sd = input$error)
    y <- input$intercept + input$slope * x + error
    #y <- 2 + 2 * x + rnorm(100)
    return(data.frame(x, y))
  })
  
  #broom::glance(lm(y ~ x, dat()))$r.squared
  output$box <- flexdashboard::renderGauge({
    # dat() %>% 
    #   lm(y ~ x, .) %>%
    #   broom::glance() %>%
    #   mutate(r.squared = r.squared *100 %>% round(1)) %>% 
    #   gauge(.$r.squared, min = 0, max = 100, symbol = '%', label = paste("R squared"))
    gauge(round(broom::glance(lm(y ~ x, dat()))$r.squared*100,1), min = 0, max = 100, symbol = '%', label = paste("R squared"))
  })
  
  #output$lm1 <- renderUI(HTML(stargazer::stargazer(lm(y ~ x, dat()), type="html")))
  output$lm1 <- renderUI({
    ##nn <- data.frame(x = rnorm(100), y = rnorm(100)) %>%
    nn <- dat() %>%
      lm(y ~ x, .) %>%
      sjt.lm(show.std = T, show.se = F, show.ci = F, use.viewer = F)
    
    nn$knitr %>% 
      HTML()
  })
  
  output$plot <- scatterD3::renderScatterD3({
    library(scatterD3)
    scatterD3(data = dat(), x = x, y = y, 
              lines = data.frame(
                slope = input$slope, 
                intercept = input$intercept
              ), 
              transitions = T)
    #hchart(dat, "point", hcaes(x, y))  %>%
    #hc_add_series(predictions, type = "line")
  })
  
  output$vis <- renderText({ "Hello Second Tab :D"})
  
}

shinyApp(ui, server)