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
        puntaje <- shiny::reactive({
            grado <- input$grado
            
            dplyr::case_when(
                grado == "Doctor" ~ 10,
                grado == "Magister" ~ 6,
                grado ==  "Título profesional"~ 4,
                grado == "Bachiller o egresado" ~ 2,
                grado == "Constancia de matrícula (Estudiante)" ~ 1,
                TRUE ~ 0
                )
        })
        
        list(
            grado = shiny::reactive(input$grado),
            puntaje_grado = shiny::reactive(puntaje())
        )
        
    })
}

mod_formacion_App <- function(){
    ui <- shiny::fluidPage(
        mod_formacion_UI("myTestId")
    )
    server <- function(input, output, session) {
        mod_formacion_Server("myTestId")
    }
    shiny::shinyApp(ui, server)
}

# mod_formacion_App()
