library(googlesheets4)
library(dplyr)
library(glue)
library(stringr)
library(purrr)

# Arma la lista de autores de una charla a partir de las columnas autor_1_nombre/autor_1_afiliacion etc... más el texto libre de la columna mas_autores (una persona por línea, separadas por líneas en blanco, en formato "Nombre, Afiliación").
formatear_autores <- function(fila) {
  # Arma "<strong>Nombre</strong> <em>(Afiliación)</em>", u omite la  afiliación si viene vacía.
  formatear_persona <- function(nombre, afiliacion) {
    if (afiliacion != "") {
      glue("<strong>{nombre}</strong> <em>({afiliacion})</em>")
    } else {
      glue("<strong>{nombre}</strong>")
    }
  }

  autores_principales <- map_chr(1:5, function(i) {
    nombre <- fila[[glue("autor_{i}_nombre")]]
    afiliacion <- fila[[glue("autor_{i}_afiliacion")]]

    if (is.na(nombre) || str_trim(nombre) == "") {
      return(NA_character_)
    }

    formatear_persona(
      str_trim(nombre),
      if (!is.na(afiliacion)) str_trim(afiliacion) else ""
    )
  })
  autores_principales <- autores_principales[!is.na(autores_principales)]

  mas_autores <- fila[["mas_autores"]]
  autores_extra <- character(0)
  if (!is.na(mas_autores) && str_trim(mas_autores) != "") {
    extra <- str_split(mas_autores, "\n+")[[1]]
    extra <- str_trim(extra)
    extra <- extra[extra != ""]

    autores_extra <- map_chr(extra, function(persona) {
      partes <- str_trim(str_split(persona, ",", n = 2)[[1]])
      formatear_persona(partes[1], if (length(partes) == 2) partes[2] else "")
    })
  }

  paste(c(autores_principales, autores_extra), collapse = "; ")
}

# Genera el bloque HTML de una charla individual (título, autores y resumen colapsable).
generar_div_charla <- function(fila) {
  titulo <- str_trim(fila$titulo)
  autores <- formatear_autores(fila)
  resumen <- fila$resumen

  html <- '<div class="charla">\n'
  html <- paste0(html, '  <div class="charla-titulo">', titulo, '</div>\n')

  if (autores != "") {
    html <- paste0(html, '  <div class="charla-autores">', autores, '</div>\n')
  }

  if (!is.na(resumen) && str_trim(resumen) != "") {
    parrafos <- str_split(str_trim(resumen), "\n{2,}")[[1]]
    resumen_html <- paste0('<p>', parrafos, '</p>', collapse = "\n")
    html <- paste0(
      html,
      '  <details class="bio-toggle charla-resumen">\n',
      '    <summary>Ver resumen</summary>\n',
      '    ', resumen_html, '\n',
      '  </details>\n'
    )
  }

  html <- paste0(html, '</div>')
  html
}

# Formatea la hora de una sesión como "HH:MM<br>(UTC-4)"

formatear_hora <- function(hora) {
  hh_mm <- str_extract(format(hora), "\\d{1,2}:\\d{2}")
  glue("{hh_mm}<br>(UTC-4)")
}

# Genera el bloque HTML de una sesión (horario + título de la sesión + las charlas asociadas, si las hay).
generar_div_sesion <- function(fila_sesion, charlas) {
  hora <- formatear_hora(fila_sesion$hora)
  titulo_sesion <- fila_sesion$sesion
  id_objetivo <- str_trim(as.character(fila_sesion$id_sesion))

  charlas_sesion <- if (!is.na(id_objetivo) && id_objetivo != "") {
    charlas |>
      filter(str_trim(as.character(id_sesion)) == id_objetivo) |>
      arrange(orden)
  } else {
    charlas[0, ]
  }

  html_charlas <- if (nrow(charlas_sesion) > 0) {
    map_chr(seq_len(nrow(charlas_sesion)), \(i) generar_div_charla(charlas_sesion[i, ])) |>
      map_chr(\(charla_html) paste0('      ', charla_html, '\n')) |>
      paste(collapse = "")
  } else {
    ""
  }

  html <- '  <div class="cronograma-row">\n'
  html <- paste0(html, '    <div class="horario">', hora, '</div>\n')
  html <- paste0(html, '    <div class="sesion">\n')
  html <- paste0(html, '      <div class="sesion-titulo">', titulo_sesion, '</div>\n')
  html <- paste0(html, html_charlas)
  html <- paste0(html, '    </div>\n')
  html <- paste0(html, '  </div>\n')
  html
}

# Genera el cronograma completo (HTML) de un día a partir de la hoja "sesiones" (horarios) y la hoja "datos-para-sitio-web" (detalle de cada charla).
generar_html_programa <- function(sesiones, charlas, dia_filtro) {
  datos_dia <- sesiones |>
    filter(dia == dia_filtro) |>
    arrange(hora)

  html_sesiones <- if (nrow(datos_dia) > 0) {
    map_chr(seq_len(nrow(datos_dia)), \(i) generar_div_sesion(datos_dia[i, ], charlas)) |>
      paste(collapse = "")
  } else {
    ""
  }

  paste0('<div class="cronograma-container">\n', html_sesiones, '</div>')
}
