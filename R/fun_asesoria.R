get_puntaje_asesoria <- function(n_doct = 0, n_mag = 0, n_bach = 0) {
    puntaje <- (n_doct*2)+(n_mag*1)+(n_bach*0.5)
    puntaje_calculado <- if (puntaje > 10) 10 else as.double(puntaje)
    return(puntaje_calculado)
}