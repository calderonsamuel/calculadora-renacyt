produccion_UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h4("B. Artículos científicos en revistas indizadas"),
        numericInput(inputId = ns("articulos_q1"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q1 Scimago o JCR)",
                     min = 0,
                     value = 0),
        numericInput(inputId = ns("articulos_q2"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q2 Scimago o JCR)",
                     min = 0,
                     value = 0),
        numericInput(inputId = ns("articulos_q3"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q3 Scimago o JCR)",
                     min = 0,
                     value = 0),
        numericInput(inputId = ns("articulos_q4"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q4 Scimago o JCR)",
                     min = 0,
                     value = 0),
        numericInput(inputId = ns("conference"),
                     label = "Conference Proceeding (Scopus o WoS)/Scielo",
                     min = 0,
                     value = 0),
        tags$h4("C. Registros de propiedad intelectual, condedidas y registradas en INDECOPI, SCOPUS u otras"),
        numericInput(inputId = ns("patente_invencion"),
                     label = "Patente de invención o Certificado de Obtentor o Paquete Tecnológico",
                     min = 0,
                     value = 0),
        numericInput(inputId = ns("patente_modelo"),
                     label = "Patente de modelo de utilidad o certificado de derecho de autor por software",
                     min = 0,
                     value = 0),
        tags$h4("D. Publicaciones de libros y/o capitulos de libro indizados"),
        numericInput(ns("libros"), "Nº de libros", min = 0, value = 0),
        numericInput(ns("capitulos"), "Nº de capitulos de libro", min = 0, value = 0),
        tags$h4("E. Índice H Scopus"),
        selectInput(ns("indice_h"), "¿Valor del índice H > 10?", 
                    choices = c("No", "Sí"),
                    selected = "No")
    )
}

produccion_Server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        puntaje_articulos <- reactive({
            q1 <- input$articulos_q1  * 5
            q2 <- input$articulos_q2  * 4
            q3 <- input$articulos_q3  * 3
            q4 <- input$articulos_q4  * 2
            conference <- if_else(input$conference > 10, 10, as.double(input$conference))
            
            q1 + q2 + q3 + q4 + conference
        })
        
        puntaje_patentes <- reactive({
            (input$patente_invencion * 3) + (input$patente_modelo * 1)
        }) 
        
        puntaje_libros <- reactive({
            puntaje <- (input$libros * 2) + (input$capitulos * 1)
            if_else(puntaje > 10, 10, as.double(puntaje))
        })
        
        list(
            puntaje_articulos = reactive(puntaje_articulos()),
            puntaje_patentes = reactive(puntaje_patentes()),
            puntaje_libros = reactive(puntaje_libros()),
            indice_h = reactive(input$indice_h)
        )
        
    })
}

produccion_App <- function(){
    ui <- fluidPage(
        produccion_UI("myTestId"),
        textOutput("test")
    )
    server <- function(input, output, session) {
        produccion <- produccion_Server("myTestId")
        output$test <- renderText(produccion$puntaje_libros())
    }
    shinyApp(ui, server)
}

# produccion_App()
