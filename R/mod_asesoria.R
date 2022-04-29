mod_asesoria_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tags$h4("F. Haber asesorado o co-asesorado tesis sustentadas y aprobadas"),
        shiny::numericInput(ns("doctor"), "Para obtención de grado Doctor", value = 0, min = 0),
        shiny::numericInput(ns("magister"), "Para obtención de grado Magister", value = 0, min = 0),
        shiny::numericInput(ns("bachiller"), "Para obtención de grado Bachiller o Título profesional", value = 0, min = 0)
    )
}

mod_asesoria_Server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        puntaje_asesoria <- shiny::reactive({
            puntaje <- (input$doctor * 2) + (input$magister * 1) + (input$bachiller * 0.5)
            dplyr::if_else(puntaje > 10, 10, as.double(puntaje))
        })
        
        list(
            puntaje_asesoria = shiny::reactive(puntaje_asesoria())
        )
    })
}

mod_asesoria_App <- function(){
    ui <- shiny::fluidPage(
        mod_asesoria_UI("myTestId"),
        shiny::textOutput("test")
    )
    server <- function(input, output, session) {
        asesoria <- mod_asesoria_Server("myTestId")
        output$test <- shiny::renderText(asesoria$puntaje_asesoria())
    }
    shiny::shinyApp(ui, server)
}

# mod_asesoria_App()
