get_puntaje_articulos <- function(q1 = 0, q2 = 0, q3 = 0, 
                                  q4 = 0, conf = 0) {
    
    p_q1 <- q1 * 5
    p_q2 <- q2 * 4
    p_q3 <- q3 * 3
    p_q4 <- q4 * 2
    p_conf <- if (conf > 10) 10 else as.double(conf)
    
    p_q1 + p_q2 + p_q3 + p_q4 + p_conf
}

get_puntaje_patentes <- function(pat_invencion = 0, pat_modelo = 0) {
    (pat_invencion * 3) + (pat_modelo * 1)
}

get_puntaje_libros <- function(n_libros = 0, n_cap = 0) {
    puntaje <- (n_libros * 2) + (n_cap * 1)
    if (puntaje > 10) 10 else as.double(puntaje)
}

get_puntaje_produccion <- function(q1 = 0, q2 = 0, q3 = 0, q4 = 0, conf = 0,
                                   pat_invencion = 0, pat_modelo = 0,
                                   n_libros = 0, n_cap = 0) {
    
    puntaje_articulos <- get_puntaje_articulos(q1, q2, q3, q4, conf)
    puntaje_patentes <- get_puntaje_patentes(pat_invencion, pat_modelo)
    puntaje_libros <- get_puntaje_libros(n_libros, n_cap)
    
    puntaje_articulos + puntaje_patentes + puntaje_libros
}
