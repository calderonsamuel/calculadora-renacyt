library(shiny)
library(tidyverse)
library(flextable)

ui <- navbarPage(
    title = "Calculadora RENACYT",
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    tabPanel(
        title = "Inicio",
        navlistPanel(
            tabPanel(
                title = "Presentación",
                tags$h2("Calculadora de calificación RENACYT"),
                includeMarkdown("presentacion.Rmd")
            ),
            tabPanel(
                title = "Sobre la calculadora",
                includeMarkdown("sobre-calculadora.Rmd")
            ),
            tabPanel(
                title = "Sobre el autor",
                includeMarkdown("sobre-autor.Rmd")
            )
        )
    ),
    tabPanel(
        title = "Calculadora",
        sidebarLayout(
            sidebarPanel(
                puntajes_input("puntajes")
            ),
            mainPanel(
                puntajes_output("puntajes")
            )
        )
    )
)

server <- function(input, output, session) {
    puntajes_Server("puntajes")
}

shinyApp(ui, server)