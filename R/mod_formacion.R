mod_formacion_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tags$h4("A. Grado académico (registrado en SUNEDU o MINEDU)"),
        shiny::selectInput(inputId = ns("grado"), 
                    label = NULL, 
                    choices = c("Doctor", 
                                "Magister", 
                                "Título profesional", 
                                "Bachiller o egresado",
                                "Constancia de matrícula (Estudiante)",
                                "Ninguno"),
                    selected = "Ninguno")
    )
}

mod_formacion_Server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        
        list(
            grado = shiny::reactive(input$grado),
            puntaje_formacion = shiny::reactive(
                get_puntaje_formacion(input$grado)
            )
        )
        
    })
}

mod_formacion_App <- function(){
    ui <- shiny::fluidPage(
        mod_formacion_UI("myTestId"),
        verbatimTextOutput("test")
    )
    server <- function(input, output, session) {
        puntos <- mod_formacion_Server("myTestId")
        output$test <- renderPrint(puntos$puntaje_grado())
    }
    shiny::shinyApp(ui, server)
}

# mod_formacion_App()
