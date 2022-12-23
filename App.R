library("shiny")
library("bslib")

custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B"
)

titleApp <- "MarketCar"

headerPanel <- navbarPage(titleApp, tabPanel("Khu vực 25"),tabPanel("Khu vực 41"),tabPanel("Về chúng tôi"))

header <- htmlTemplate("./components/nav_header.html", yeld = headerPanel)

layout <- bootstrapPage(htmlTemplate("./teamplates/main.html", title="Market car sumary", header = header), theme = custom_theme)


server <- function(input, output) {
  
}

shinyApp(ui = layout, server)