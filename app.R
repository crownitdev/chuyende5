library("shiny")
library("bslib")
source("./utils/boot.R")

custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B"
)


titleApp <- "MarketCar"
# Components
table25 <- htmlTemplate("./components/table.html", table = dataTableOutput('table25'), title="Dữ liệu cleansing")
table41 <- htmlTemplate("./components/table.html", table = dataTableOutput('table41'), title="Dữ liệu cleansing")
renderPlotChart <- function(strOutput, title) {
  htmlTemplate("./components/chart.html", chart = plotOutput(strOutput), title=title)
}

renderPlotChartSider <- function(strOutput, title, inputId) {
  slider <- sliderInput(inputId = inputId,
              min = 100,
              label = "",
              max = 5000,
              step = 100,
              value = 200)
  htmlTemplate("./components/chart_slider.html", slider = slider, chart = plotOutput(strOutput), title=title)
}

# Pages
region25 <- htmlTemplate(
  "./pages/home.html",
  tableClean = table25,
  charts = c(renderPlotChart('sg25YearAndPower', title = "Biểu đồ phân tán giữa sức mạnh và năm sản xuất"),
             renderPlotChart('sg25PriceAndPower', title = "Biểu đồ phân tán giữa giá xe và sức mạnh"),
             renderPlotChart('sg25YearAndPrice', title = "Biểu đồ phân tán giữa năm và giá"),
             renderPlotChartSider('sgn25PriceAndPower', title = textOutput('engine25'), inputId = "engine25")
            )
  )

region41 <- htmlTemplate(
  "./pages/region41.html",
  tableClean = table41,
  charts = c(renderPlotChart('sg41YearAndPower', title = "Biểu đồ phân tán giữa sức mạnh và năm sản xuất"),
             renderPlotChart('sg41PriceAndPower', title = "Biểu đồ phân tán giữa giá xe và sức mạnh"),
             renderPlotChart('sg41YearAndPrice', title = "Biểu đồ phân tán giữa năm và giá"),
             renderPlotChartSider('sgn41PriceAndPower', title = textOutput('engine41'), inputId = "engine41")
  )
)

about <- htmlTemplate("./pages/about.html")

headerPanel <- navbarPage(titleApp, tabPanel("Khu vực 25", region25),tabPanel("Khu vực 41", region41),tabPanel("Về chúng tôi", about))
header <- htmlTemplate("./components/nav_header.html", yeld = headerPanel)

# Layouts
layout <- bootstrapPage(htmlTemplate("./teamplates/main.html", title="Market car sumary", header = header), theme = custom_theme)


server <- function(input, output) {
  output$table25 <- renderDataTable(data25Clean, 
                                 options = list(pageLength = 15, info = FALSE,
                                                autoWidth = TRUE,
                                                scrollX = TRUE,
                                                fixedHeader = TRUE,
                                                lengthMenu = list(c(15, -1), c("15", "All"))
                                              )
                                 )
  output$table41 <- renderDataTable(data41Clean, 
                                    options = list(pageLength = 15, info = FALSE,
                                                   autoWidth = TRUE,
                                                   scrollX = TRUE,
                                                   fixedHeader = TRUE,
                                                   lengthMenu = list(c(15, -1), c("15", "All"))
                                    )
  )
  output$sg25YearAndPower <- renderPlot(getCompare(visual25Data, "year", "power"))
  output$sg25PriceAndPower <- renderPlot(getCompare(visual25Data, "price", "power"))
  output$sg25YearAndPrice <- renderPlot(getCompare(visual25Data, "year", "price"))
  
  output$sg41YearAndPower <- renderPlot(getCompare(visual41Data, "year", "power"))
  output$sg41PriceAndPower <- renderPlot(getCompare(visual41Data, "price", "power"))
  output$sg41YearAndPrice <- renderPlot(getCompare(visual41Data, "year", "price"))
  
  
  
  observe({
    output$sgn25PriceAndPower <- renderPlot({getEngine(visual25Data, input$engine25)})
    output$engine25 <- renderText(paste("Mật độ tập trung của xe có mốc ",input$engine25, " mã lực đổ lại"))
    
    output$sgn41PriceAndPower <- renderPlot({getEngine(visual41Data, input$engine41)})
    output$engine41 <- renderText(paste("Mật độ tập trung của xe có mốc ",input$engine41, " mã lực đổ lại"))
    dataPredict25 <- predictData25(input$powerPredict25)
    output$confidencePrice25 <- renderText(dataPredict25[1])
    output$predictPrice25 <- renderText(dataPredict25[2])
    
    dataPredict41 <- predictData41(input$powerPredict41)
    output$confidencePrice41 <- renderText(dataPredict41[1])
    output$predictPrice41 <- renderText(dataPredict41[2])
    
  })
    output$plotLinerPredict25 <- renderPlot(plotPredict25)
    output$plotLinerPredict41 <- renderPlot(plotPredict41)
}

shinyApp(ui = layout, server)