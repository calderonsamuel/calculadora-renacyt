get_puntaje_total <- function(p_formacion = 0,
                              p_produccion = 0,
                              p_asesoria = 0) {
    
    p_formacion + p_produccion + p_asesoria
}

get_calificacion <- function(puntaje_produccion = 0, 
                             puntaje_formacion = 0, 
                             puntaje_total = 0, 
                             indice_h = "No") {
    dplyr::case_when(
        puntaje_produccion == 0 ~ "No califica: Requiere al menos un ítem en Producción",
        puntaje_formacion == 1 & puntaje_produccion < 9 ~ "No califica: Estudiantes requieren 9 en producción",
        puntaje_formacion > 1 & puntaje_produccion< 6  ~ "No califica: Requiere al menos 6 en producción",
        puntaje_total < 10 ~ "No califica: Requiere al menos 10 en puntaje total",
        puntaje_total <= 24 ~ "Sí califica: Nivel VII",
        puntaje_total <= 34 ~ "Sí califica: Nivel VI",
        puntaje_total <= 49 ~ "Sí califica: Nivel V",
        puntaje_total <= 69 ~ "Sí califica: Nivel IV",
        puntaje_total <= 99 ~ "Sí califica: Nivel III",
        puntaje_total <= 159 ~ "Sí califica: Nivel II",
        puntaje_total <= 199 ~ "Sí califica: Nivel I",
        indice_h == "Sí" ~ "Investigador Distinguido",
        TRUE ~ "Nivel I"
    )
}
