mod_puntajes_input <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tabsetPanel(
            shiny::tabPanel(
                title = "Formación",
                mod_formacion_UI(ns("formacion"))
            ),
            shiny::tabPanel(
                title = "Producción",
                mod_produccion_UI(ns("produccion"))
            ),
            shiny::tabPanel(
                title = "Asesoría",
                mod_asesoria_UI(ns("asesoria"))
            )
        )
    )
}

mod_puntajes_output <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::tags$h2("Puntaje obtenido"),
        shiny::uiOutput(ns("resultados")),
        shiny::tags$h2("Calificación"),
        shiny::textOutput(ns("calificacion")),
        shiny::tags$p("Considerar que para que el registro prospere se requiere al menos un item de producción en los últimos tres años")
    )
}

mod_puntajes_Server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        
        formacion <- mod_formacion_Server("formacion")
        produccion <- mod_produccion_Server("produccion")
        asesoria <- mod_asesoria_Server("asesoria")
        
        puntaje_formacion <- shiny::reactive(formacion$puntaje_formacion())
        puntaje_produccion <- shiny::reactive(produccion$puntaje_produccion())
        puntaje_asesoria <- shiny::reactive(asesoria$puntaje_asesoria())
        
        puntaje_total <- shiny::reactive(
            get_puntaje_total(
                p_formacion = puntaje_produccion(),
                p_produccion = puntaje_produccion(),
                p_asesoria = puntaje_asesoria()
            )
        )
        
        data_resultados <- shiny::reactive({
            
            # data.frame(
            #     Criterio = c("Formación", "Producción total", "Asesoría", "Total"),
            #     Puntaje = c(puntaje_formacion(), puntaje_produccion(), puntaje_asesoria(), puntaje_total())
            # )
            dplyr::tribble(
                ~"Criterio", ~"Puntaje",
                "Formación", puntaje_formacion(),
                "Producción total", puntaje_produccion(),
                "Asesoría", puntaje_asesoria(),
                "Total", puntaje_total()
            )
        })
        
        calificacion <- shiny::reactive({
            get_calificacion(
                puntaje_produccion = puntaje_produccion(),
                puntaje_formacion = puntaje_formacion(),
                puntaje_total = puntaje_total(),
                indice_h = produccion$indice_h()
            )
        })
        
        output$resultados <- shiny::renderUI({
            data_resultados() |> 
            flextable::flextable() |> 
            flextable::theme_box()  |> 
            flextable::set_table_properties(layout = "autofit") |> 
            flextable::htmltools_value()
        })
        
        output$calificacion <- shiny::renderText(calificacion())
    })
}

mod_puntajes_App <- function(){
    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                mod_puntajes_input("myTestId")
            ),
            shiny::mainPanel(
                mod_puntajes_output("myTestId")
            )
        )
    )
    
    server <- function(input, output, session) {
        mod_puntajes_Server("myTestId")
    }
    shiny::shinyApp(ui, server)
}

# mod_puntajes_App()
