puntajes_input <- function(id) {
    ns <- NS(id)
    tagList(
        tabsetPanel(
            tabPanel(
                title = "Formación",
                formacion_UI(ns("formacion"))
            ),
            tabPanel(
                title = "Producción",
                produccion_UI(ns("produccion"))
            ),
            tabPanel(
                title = "Asesoría",
                asesoria_UI(ns("asesoria"))
            )
        )
    )
}

puntajes_output <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Puntaje obtenido"),
        uiOutput(ns("resultados")),
        tags$h2("Calificación"),
        textOutput(ns("calificacion")),
        tags$p("Considerar que para que el registro prospere se requiere al menos un item de producción en los últimos tres años")
    )
}

puntajes_Server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        formacion <- formacion_Server("formacion")
        produccion <- produccion_Server("produccion")
        asesoria <- asesoria_Server("asesoria")
        
        puntaje_formacion <- reactive(formacion$puntaje_grado())
        
        puntaje_produccion <- reactive({
            articulos <- produccion$puntaje_articulos()
            patentes <- produccion$puntaje_patentes()
            libros <- produccion$puntaje_libros()
            
            articulos + patentes + libros
        })
        
        puntaje_asesoria <- reactive(asesoria$puntaje_asesoria())
        
        puntaje_total <- reactive(puntaje_formacion() + 
                                      puntaje_produccion() + 
                                      puntaje_asesoria())
        
        data_resultados <- reactive({
            tribble(
                ~"Criterio", ~"Puntaje",
                "Formación", puntaje_formacion(),
                "Producción total", puntaje_produccion(),
                "Asesoría", puntaje_asesoria(),
                "Total", puntaje_total()
            )
        })
        
        calificacion <- reactive({
            case_when(
                puntaje_produccion() == 0 ~ "No califica: Requiere al menos un ítem en Producción",
                puntaje_formacion() == 1 & puntaje_produccion() < 9 ~ "No califica: Estudiantes requieren 9 en producción",
                puntaje_formacion() > 1 & puntaje_produccion() < 6  ~ "No califica: Requiere al menos 6 en producción",
                puntaje_total() < 10 ~ "No califica: Requiere al menos 10 en puntaje total",
                puntaje_total() <= 24 ~ "Sí califica: Nivel VII",
                puntaje_total() <= 34 ~ "Sí califica: Nivel VI",
                puntaje_total() <= 49 ~ "Sí califica: Nivel V",
                puntaje_total() <= 69 ~ "Sí califica: Nivel IV",
                puntaje_total() <= 99 ~ "Sí califica: Nivel III",
                puntaje_total() <= 159 ~ "Sí califica: Nivel II",
                puntaje_total() <= 199 ~ "Sí califica: Nivel I",
                produccion$indice_h() == "Sí" ~ "Investigador Distinguido",
                TRUE ~ "Nivel I"
            )
        })
        
        output$resultados <- renderUI({
            data_resultados() %>% 
                flextable() %>% 
                theme_box() %>% 
                set_table_properties("autofit") %>% 
                htmltools_value()
        })
        
        output$calificacion <- renderText(calificacion())
    })
}

puntajes_App <- function(){
    ui <- fluidPage(
                sidebarLayout(
                        sidebarPanel(
                                puntajes_input("myTestId")
                        ),
                        mainPanel(
                                puntajes_output("myTestId")
                        )
                )
    )
    
    server <- function(input, output, session) {
        puntajes_Server("myTestId")
    }
    shinyApp(ui, server)
}

# puntajes_App()
