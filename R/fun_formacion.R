get_puntaje_formacion <- function(grado_obtenido = "Ninguno") {
    switch(grado_obtenido,
           "Doctor" = 10,
           "Magister" = 6,
           "Título profesional" = 4,
           "Bachiller o egresado" = 2,
           "Constancia de matrícula (Estudiante)" = 1,
           "Ninguno" = 0)
}