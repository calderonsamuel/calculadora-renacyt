get_puntaje_total <- function(p_formacion = 0,
                              p_produccion = 0,
                              p_asesoria = 0) {
    
    p_formacion + p_produccion + p_asesoria
}

get_calificacion <- function(puntaje_produccion = 0, 
                             puntaje_formacion = 0, 
                             puntaje_total = 0, 
                             indice_h = "No") {
    
    if (puntaje_produccion == 0) return("No califica: Requiere al menos un ítem en Producción")
    if (puntaje_formacion == 1 & puntaje_produccion < 9) return("No califica: Estudiantes requieren 9 en producción")
    if (puntaje_formacion > 1 & puntaje_produccion < 6) return("No califica: Requiere al menos 6 en producción")
    if (puntaje_total < 10) return("No califica: Requiere al menos 10 en puntaje total")
    if (puntaje_total <= 24) return("Sí califica: Nivel VII")
    if (puntaje_total <= 34) return("Sí califica: Nivel VI")
    if (puntaje_total <= 49) return("Sí califica: Nivel V")
    if (puntaje_total <= 69) return("Sí califica: Nivel IV")
    if (puntaje_total <= 99) return("Sí califica: Nivel III")
    if (puntaje_total <= 159) return("Sí califica: Nivel II")
    if (puntaje_total <= 199) return("Sí califica: Nivel I")
    if (indice_h == "Sí") return("Investigador Distinguido") else "Sí califica: Nivel I"
}
