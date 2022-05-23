mod_produccion_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tags$h4("B. Artículos científicos en revistas indizadas"),
        shiny::numericInput(inputId = ns("articulos_q1"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q1 Scimago o JCR)",
                     min = 0,
                     value = 0),
        shiny::numericInput(inputId = ns("articulos_q2"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q2 Scimago o JCR)",
                     min = 0,
                     value = 0),
        shiny::numericInput(inputId = ns("articulos_q3"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q3 Scimago o JCR)",
                     min = 0,
                     value = 0),
        shiny::numericInput(inputId = ns("articulos_q4"), 
                     label = "Nº de Art. cient. Scopus o Web of Science (Q4 Scimago o JCR)",
                     min = 0,
                     value = 0),
        shiny::numericInput(inputId = ns("conference"),
                     label = "Conference Proceeding (Scopus o WoS)/Scielo",
                     min = 0,
                     value = 0),
        shiny::tags$h4("C. Registros de propiedad intelectual, concedidas y registradas en INDECOPI, SCOPUS u otras"),
        shiny::numericInput(inputId = ns("patente_invencion"),
                     label = "Patente de invención o Certificado de Obtentor o Paquete Tecnológico",
                     min = 0,
                     value = 0),
        shiny::numericInput(inputId = ns("patente_modelo"),
                     label = "Patente de modelo de utilidad o certificado de derecho de autor por software",
                     min = 0,
                     value = 0),
        shiny::tags$h4("D. Publicaciones de libros y/o capítulos de libro indizados"),
        shiny::numericInput(ns("libros"), "Nº de libros", min = 0, value = 0),
        shiny::numericInput(ns("capitulos"), "Nº de capítulos de libro", min = 0, value = 0),
        shiny::tags$h4("E. Índice H Scopus"),
        shiny::selectInput(ns("indice_h"), "¿Valor del índice H > 10?", 
                    choices = c("No", "Sí"),
                    selected = "No")
    )
}

mod_produccion_Server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        
        puntaje_produccion <- shiny::reactive({
            get_puntaje_produccion(
                q1 = input$articulos_q1,
                q2 = input$articulos_q2,
                q3 = input$articulos_q3,
                q4 = input$articulos_q4, 
                conf = input$conference,
                pat_invencion = input$patente_invencion,
                pat_modelo = input$patente_modelo,
                n_libros = input$libros, 
                n_cap = input$capitulos
            )
        })
        
        list(
            puntaje_produccion = shiny::reactive(puntaje_produccion()),
            indice_h = shiny::reactive(input$indice_h)
        )
        
    })
}

mod_produccion_App <- function(){
    ui <- shiny::fluidPage(
        mod_produccion_UI("myTestId"),
        shiny::textOutput("test")
    )
    server <- function(input, output, session) {
        produccion <- mod_produccion_Server("myTestId")
        output$test <- shiny::renderText(produccion$puntaje_produccion())
    }
    shiny::shinyApp(ui, server)
}

# mod_produccion_App()
