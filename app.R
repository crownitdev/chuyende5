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
headerPanel <- navbarPage(titleApp, tabPanel("Khu vực 25"),tabPanel("Khu vực 41"),tabPanel("Về chúng tôi"))
header <- htmlTemplate("./components/nav_header.html", yeld = headerPanel)
table <- htmlTemplate("./components/table.html", table = dataTableOutput('table'), title="Dữ liệu cleansing")
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


chartYearVsPow <- renderPlotChart('sgYearAndPower', title = "Biểu đồ phân tán giữa sức mạnh và năm sản xuất")
chartPriceVsPow <- renderPlotChart('sgPriceAndPower', title = "Biểu đồ phân tán giữa giá xe và sức mạnh")
chartYearAndPrice <- renderPlotChart('sgYearAndPrice', title = "Biểu đồ phân tán giữa năm và giá")
chartEngine <- renderPlotChartSider('sgnPriceAndPower', title = textOutput('engine'), inputId = "engine")
# Pages
page <- htmlTemplate(
  "./pages/home.html",
  tableClean = table,
  charts = c(chartYearVsPow,
             chartPriceVsPow,
            chartYearAndPrice,
            chartEngine
            ),
  predictDataPrice = textOutput('confidence', inline = TRUE)
  )

# Layouts
layout <- bootstrapPage(htmlTemplate("./teamplates/main.html", title="Market car sumary", header = header, page = page), theme = custom_theme)

server <- function(input, output) {
  output$table = renderDataTable(dataClean, 
                                 options = list(pageLength = 15, info = FALSE,
                                                autoWidth = TRUE,
                                                scrollX = TRUE,
                                                fixedHeader = TRUE,
                                                lengthMenu = list(c(15, -1), c("15", "All")) ) )
  output$sgYearAndPower = renderPlot(getCompare("year", "power"))
  output$sgPriceAndPower = renderPlot(getCompare("price", "power"))
  output$sgYearAndPrice = renderPlot(getCompare("year", "price"))
  
  
  observe({
    valueEngine <- input$engine
    if(!is.na(valueEngine)){
      output$sgnPriceAndPower = renderPlot({getEngine(valueEngine)})
      output$engine <- renderText(paste("Mật độ tập trung của xe có mốc ",valueEngine, " mã lực đổ lại"))
    }
    dataPredict <- predictData(input$powerPredict)
    output$confidencePrice <- renderText(dataPredict[1])
    output$predictPrice <- renderText(dataPredict[2])
  })
  output$plotLinerPredict <- renderPlot(plotPredict)
}

shinyApp(ui = layout, server)