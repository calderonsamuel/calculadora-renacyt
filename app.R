library(shiny)
library(markdown) # tratando de solucionar problema en deploy a shinyapps.io
# library(tidyverse)
# library(flextable)

ui <- shiny::navbarPage(
    shiny::tags$head(shiny::includeHTML(("google-analytics.html"))),
    title = "Calculadora RENACYT",
    selected = "inicio",
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
        value = "inicio",
        shiny::navlistPanel(
            shiny::tabPanel(
                title = "Presentación",
                shiny::tags$h2("Calculadora de calificación RENACYT"),
                shiny::includeMarkdown("presentacion.Rmd")
            ),
            shiny::tabPanel(
                title = "Sobre la calculadora",
                shiny::includeMarkdown("sobre-calculadora.Rmd")
            ),
            shiny::tabPanel(
                title = "Sobre el autor",
                shiny::includeMarkdown("sobre-autor.Rmd")
            )
        )
    ),
    tabPanel(
        title = "Calculadora",
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                mod_puntajes_input("puntajes")
            ),
            shiny::mainPanel(
                mod_puntajes_output("puntajes")
            )
        )
    )
)

server <- function(input, output, session) {
    mod_puntajes_Server("puntajes")
}

shiny::shinyApp(ui, server)