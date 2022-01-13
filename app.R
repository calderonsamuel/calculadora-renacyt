# library(shiny)
# library(tidyverse)
# library(flextable)

ui <- shiny::navbarPage(
    shiny::tags$head(includeHTML(("google-analytics.html"))),
    title = "Calculadora RENACYT",
    theme = bslib::bs_theme(version = 4, 
                            bootswatch = "minty",
                            "link-color" = "#770",
                            "link-hover-color" = "#AA6",
                            primary = "#707070",
                            bg = "#f6f4f3",
                            fg = "black",
                            "navbar-light-bg" = "#f6eee9",
                            "navbar-light-active-color" = "maroon",
                            "navbar-light-hover-color" = "#FF3333",
                            "navbar-light-brand-color" = "maroon",
                            "component-active-bg" = "#e6ded9",
                            "navbar-brand-font-size" = "1.75rem"
                            ),
    shiny::tabPanel(
        title = "Inicio",
        shiny::navlistPanel(
            shiny::tabPanel(
                title = "Presentación",
                tags$h2("Calculadora de calificación RENACYT"),
                includeMarkdown("presentacion.Rmd")
            ),
            shiny::tabPanel(
                title = "Sobre la calculadora",
                includeMarkdown("sobre-calculadora.Rmd")
            ),
            shiny::tabPanel(
                title = "Sobre el autor",
                includeMarkdown("sobre-autor.Rmd")
            )
        )
    ),
    tabPanel(
        title = "Calculadora",
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                puntajes_input("puntajes")
            ),
            shiny::mainPanel(
                puntajes_output("puntajes")
            )
        )
    )
)

server <- function(input, output, session) {
    puntajes_Server("puntajes")
}

shiny::shinyApp(ui, server)