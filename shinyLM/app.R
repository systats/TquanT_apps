library(shiny)
### once install package loader called pacman
# install.packages("pacman")

### install & load libraries from CRAN
pacman::p_load(
  shiny, sjPlot, 
  dplyr, scatterD3, 
  plotly, reshape2
)

### install & load from github (dev versions)
pacman::p_load_gh(
  "Appsilon/shiny.semantic",
  "Appsilon/semantic.dashboard", 
  "daattali/shinyjs",
  dependencies = T#, update = T
)
# devtools::install_github("Appsilon/shiny.semantic", ref = "develop")
# devtools::install_github("nteetor/dull")

### for shiny server
# library(shiny)
# library(shiny.semantic)
# library(semantic.dashboard)
# library(shinyWidgets)
# library(dplyr)
# library(sjPlot)
# library(flexdashboard)
# library(scatterD3)
# library(sjPlot)
# library(plotly)
# library(reshape2)

source("helper.R")

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
  dashboardBody(
  # shiny::div(
  #   class = "pusher",
  #   style="min-height: 100vh;",
    tabItems(
      selected = 1,
      tabItem(
        tabName = "sim_lin_reg",
        column(
          width = 11,
          box(
            title = "Scatterplot", 
            color = "blue", 
            title_side = "top", 
            ribbon = F,
            collapsible = F, 
            width = 11,
            br(),
            br(),
            scatterD3::scatterD3Output("plot", width = "100%"),
            br(),
            br()
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
            sliderInput("intercept", label = "Intercept", min = -10, max = 10, value = 0, step = 1, 
                        animate = animationOptions(playButton = uiicon("green play"), pauseButton = uiicon("red pause"))),
            sliderInput("slope", label = "Slope", min = -2, max = 2, value = 1, step = .1, 
                        animate = animationOptions(playButton = uiicon("green play"), pauseButton = uiicon("red pause"))),
            sliderInput("error", label = "Sigma", min = 0, max = 10, value = 2, step = .5, 
                        animate = animationOptions(playButton = uiicon("green play"), pauseButton = uiicon("red pause"))),
            sliderInput("n", label = "N", min = 10, max = 1000, step = 20, value = 100, 
                        animate = animationOptions(playButton = uiicon("green play"), pauseButton = uiicon("red pause")))
          ),
          box(
            title = "Estimation", 
            color = "red", 
            title_side = "top", 
            ribbon = F,
            collapsible = T, 
            width = 4,
            uiOutput("lm1"),
            br(),
            uiOutput("ind")
          )
        )
      ),
      tabItem(
        tabName = "multi_lin_reg",
        column(
          width = 11,
          box(
            title = "3D Scatterplot", 
            color = "blue", 
            title_side = "top", 
            ribbon = F,
            collapsible = F, 
            width = 11,
            style="min-height: 100vh;",
            br(),
            plotly::plotlyOutput("D3")
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
            sliderInput("slope1", label = "slope1", min = -2, max = 2, value = 0, step =.1)
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    set.seed(2017)
    x <- runif(input$n, 1, 7)
    y <- input$intercept + input$slope * x + rnorm(input$n, sd = input$error)
    return(data.frame(x, y))
  })
  
  ind <- reactive({
    round(broom::glance(lm(y ~ x, dat()))$r.squared*100,1) %>% as.character()
  })
  
  output$ind <- renderUI({
    tagList(
      div(class="ui green progress",
          #`data-value` = "1",
          `data-total` = "100",
          id = "example5",
          div(class="bar",
              div(class="progress")
          ),
          div(class="label",
              "Variance 'Explained'"
          )
      ),
      tags$script(paste0("$('#example5').progress({percent:", ind(), "});"))
    )
  })
  
  
  #output$lm1 <- renderUI(HTML(stargazer::stargazer(lm(y ~ x, dat()), type="html")))
  output$lm1 <- renderUI({
    ##nn <- data.frame(x = rnorm(100), y = rnorm(100)) %>%
    nn <- dat() %>%
      lm(y ~ x, .) %>%
      sjt.lm(
        show.std = T,
        show.se = F,
        show.ci = F, 
        emph.p = T, 
        p.zero = F,
        use.viewer = F, 
        cell.spacing = .25
      )

    nn$knitr %>%
      HTML()
    #nn <- data.frame(x = rnorm(100), y = rnorm(100))
    # dat() %>%
    #   lm(y ~ x, .) %>%
    #   broom::tidy(.) %>%
    #   rename(` ` = term, B = estimate, SE = std.error, p = p.value) %>%
    #   select(-statistic) %>%
    #   mutate(B = round(B, 2), SE = round(SE, 2), p = round(p, 2)) %>%
    #   mutate(p = kableExtra::cell_spec(p, "html", color = ifelse(p < .05, "green", "red"))) %>%
    #   knitr::kable(format = "html", digits = 2) %>%
    #   HTML()
  })
  
  output$plot <- scatterD3::renderScatterD3({
    library(scatterD3)
    scatterD3(
      data = dat(), 
      x = x, y = y, 
      lines = data.frame(
        slope = input$slope, 
        intercept = input$intercept
      ), 
      transitions = T
    )
    #hchart(dat, "point", hcaes(x, y))  %>%
    #hc_add_series(predictions, type = "line")
  })
  
  dat_3d <- reactive({
    x1 <- runif(100, min = 0, max = 10)
    hist(x1)
    
    x2 <- runif(100, min = 0, max = 10)
    
    y <- as.numeric(input$slope1) * x1 + x2 + rnorm(100) 
    y <- y/max(y)*10

    return(data.frame(x1, x2, y))
  })
  
  
  output$D3 <- plotly::renderPlotly({
    fit_plane_surface <- fit_plane(dat_3d())
    p <- plot_plane(dat_3d(), fit_plane_surface)
    return(p)
  })
}

shinyApp(ui, server)