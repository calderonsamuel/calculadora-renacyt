asesoria_UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h4("F. Haber asesorado o co-asesorado tesis sustentadas y aprobadas"),
        numericInput(ns("doctor"), "Para obtención de grado Doctor", value = 0, min = 0),
        numericInput(ns("magister"), "Para obtención de grado Magister", value = 0, min = 0),
        numericInput(ns("bachiller"), "Para obtención de grado Bachiller o Título profesional", value = 0, min = 0)
    )
}

asesoria_Server <- function(id) {
    moduleServer(id, function(input, output, session) {
        puntaje_asesoria <- reactive({
            puntaje <- (input$doctor * 2) + (input$magister * 1) + (input$bachiller * 0.5)
            if_else(puntaje > 10, 10, as.double(puntaje))
        })
        
        list(
            puntaje_asesoria = reactive(puntaje_asesoria())
        )
    })
}

asesoria_App <- function(){
    ui <- fluidPage(
        asesoria_UI("myTestId"),
        textOutput("test")
    )
    server <- function(input, output, session) {
        asesoria <- asesoria_Server("myTestId")
        output$test <- renderText(asesoria$puntaje_asesoria())
    }
    shinyApp(ui, server)
}

# asesoria_App()
