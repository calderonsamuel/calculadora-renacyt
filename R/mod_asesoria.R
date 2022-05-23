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
        
        list(
            puntaje_asesoria = shiny::reactive(
                get_puntaje_asesoria(
                    n_doct = input$doctor,
                    n_mag = input$magister,
                    n_bach = input$bachiller
                )
            )
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
