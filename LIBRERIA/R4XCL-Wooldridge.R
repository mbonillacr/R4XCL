#' Explora y carga sets de datos de la libreria wooldridge con una interfaz grafica.
#'
#' Esta funcion verifica si la libreria 'wooldridge' esta instalada. Si no lo
#' esta, la instala. Luego, presenta al usuario una lista de los sets de datos
#' disponibles en la libreria, permitiendole seleccionar uno de ellos para
#' cargarlo directamente en el entorno de R en la variable 'datos_cargados'.
#'
#' @return El set de datos seleccionado se carga en el entorno de R del usuario
#'   en una variable llamada 'datos_cargados'.
#'
DS_Wooldridge  <- function() {
  
  # 1. Verificar si el paquete svDialogs esta instalado
  if (!requireNamespace("svDialogs", quietly = TRUE)) {
    stop("El paquete 'svDialogs' no esta instalado. Por favor, instalelo con:\ninstall.packages('svDialogs')")
  }
  
  # 2. Verificar e instalar la libreria wooldridge si es necesario
  if (!requireNamespace("wooldridge", quietly = TRUE)) {
    svDialogs::dlg_message(
      "La libreria 'wooldridge' no esta instalada. Sera instalada ahora.",
      title = "Libreria no encontrada"
    )$res
    
    tryCatch({
      install.packages("wooldridge", dependencies = TRUE)
    },
    error = function(e) {
      svDialogs::dlg_message(
        sprintf("Error al instalar el paquete 'wooldridge': %s", e$message),
        title = "Error de instalacion",
        type = "ok",
        gui = "warning"
      )$res
      return(invisible(NULL))
    })
  }
  
  # 3. Cargar la libreria wooldridge
  library(wooldridge)
  
  # 4. Obtener la lista de sets de datos disponibles
  sets_de_datos <- data(package = "wooldridge")$results[, "Item"]
  
  if (length(sets_de_datos) == 0) {
    svDialogs::dlg_message(
      "No se encontraron sets de datos en la libreria 'wooldridge'.",
      title = "Sin sets de datos",
      type = "ok",
      gui = "warning"
    )$res
    return(invisible(NULL))
  }
  
  # 5. Presentar la lista de sets de datos al usuario para seleccion
  opcion_seleccionada <- svDialogs::dlg_list(
    choices = sets_de_datos,
    multiple = FALSE,
    title = "Seleccione un set de datos de la libreria wooldridge"
  )$res
  
  # 6. Manejar la seleccion del usuario
  if (is.null(opcion_seleccionada) || length(opcion_seleccionada) == 0) {
    svDialogs::dlg_message(
      "Seleccion de set de datos cancelada por el usuario.",
      title = "Proceso cancelado",
      type = "ok",
      gui = "info"
    )$res
    return(invisible(NULL))
  } else {
    # 7. Cargar el set de datos y guardarlo en una variable 'datos_cargados'
    tryCatch({
      # Cargar el set de datos con su nombre original
      data(list = opcion_seleccionada)
      
      # Asignar el set de datos cargado a la nueva variable
      assign("datos_cargados", get(opcion_seleccionada), envir = .GlobalEnv)
      
      # 8. Imprimir el set de datos cargado en la consola
      return(datos_cargados)
      
      # 9. Confirmar que el set de datos ha sido cargado exitosamente
      if (exists("datos_cargados")) {
        svDialogs::dlg_message(
          sprintf("¡El set de datos '%s' ha sido cargado en la variable 'datos_cargados' y se ha impreso en la consola!\n\nPuede empezar a explorarlo con:\n- str(datos_cargados)\n- head(datos_cargados)", opcion_seleccionada),
          title = "Seleccion exitosa",
          type = "ok",
          gui = "info"
        )$res
      } else {
        stop("El set de datos no pudo ser cargado.")
      }
    },
    error = function(e) {
      svDialogs::dlg_message(
        sprintf("Error al cargar el set de datos '%s': %s", opcion_seleccionada, e$message),
        title = "Error de carga",
        type = "ok",
        gui = "warning"
      )$res
    })
  }
}
