library(googlesheets4)
library(httr)
library(jsonlite)
library(base64enc)


# autentificarme con gargle con el json de la cuenta de servicio
gs4_auth(path = "C:/Users/Jorge/Documents/GitHub/cuotas-auto/limesurvey-379408-91651184e9db.json", gargle::gargle_oauth_email())


# Crear un nuevo entorno para almacenar la caché de sesión
session_cache <- new.env(parent = emptyenv())


# Función para convertir una cadena codificada en base64 en un data frame
base64_to_df <- function(x) {
  raw_csv <- rawToChar(base64enc::base64decode(x))
  return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE, sep = ";"))
}


# Función para obtener los participantes de una encuesta
get_participants <- function(iSurveyID, iStart, iLimit, bUnused, aAttributes) {
  # Poner todos los argumentos de la función en una lista para luego pasarlos a call_limer()
  params <- as.list(environment())

  # Llamar a la API de LimeSurvey con el método "list_participants"
  results <- call_limer(method = "list_participants", params = params)

  # Devolver los resultados como un data frame
  return(data.frame(results))
}

# Función para obtener las respuestas de una encuesta
get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", ...) {
  # Poner todos los argumentos de la función en una lista para luego pasarlos a call_limer()
  params <- as.list(environment())
  dots <- list(...)
  if (length(dots) > 0) params <- append(params, dots)

  # Llamar a la API de LimeSurvey con el método "export_responses"
  results <- call_limer(method = "export_responses", params = params)

  # Decodificar los resultados codificados en base64 y devolverlos como un data frame
  return(base64_to_df(unlist(results)))
}

get_session_key <- function(username = getOption('lime_username'),
                            password = getOption('lime_password')) {
  body.json = list(method = "get_session_key",
                   id = " ",
                   params = list(username = username,
                                 password = password))

  # Need to use jsonlite::toJSON because single elements are boxed in httr, which
  # is goofy. toJSON can turn off the boxing automatically, though it's not
  # recommended. They say to use unbox on each element, like this:
  #   params = list(admin = unbox("username"), password = unbox("password"))
  # But that's a lot of extra work. So auto_unbox suffices here.
  # More details and debate: https://github.com/hadley/httr/issues/159
  r <- POST(getOption("lime_api"), content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE)
  )

  session_key <- as.character(jsonlite::fromJSON(content(r,, as='text', encoding = "utf-8"))$result)
  session_cache$session_key <- session_key
  session_key
}

# Función para llamar a la API de LimeSurvey
call_limer <- function(method, params = list(), ...) {
  if (!is.list(params)) {
    stop("params must be a list.")
  }

  if (!exists("session_key", envir = session_cache)) {
    stop("You need to get a session key first. Run get_session_key().")
  }

  key.list <- list(sSessionKey = session_cache$session_key)
  params.full <- c(key.list, params)

  body.json <- list(method = method,
                    # This seems to not matter, but the API call breaks without it,
                    # so just pass nothing. ¯\_(ツ)_/¯
                    id = " ",
                    params = params.full)

  r <- httr::POST(getOption('lime_api'), httr::content_type_json(),
            body = jsonlite::toJSON(body.json, auto_unbox = TRUE), ...)

  return(jsonlite::fromJSON(httr::content(r, as='text', encoding="utf-8"))$result)   # incorporated fix by petrbouchal
}

# Función para obtener respuestas y escribir en Google Sheets
write_responses_to_sheet <- function(iSurveyID, sheet_name, url_gsheet) {
  responses <- get_responses(iSurveyID = iSurveyID)
  range_write(responses, ss = url_gsheet, sheet = sheet_name, range = "A2", col_names = FALSE)
}

release_session_key <- function() {
  call_limer(method = "release_session_key")
}

# Leer datos de autenticación desde la hoja de cálculo de Google
credentials <- range_read("1baHiY4QRisx04JUMUKNoQhhh_SZr0fLDiEEkoQMDlYA", range = "A1:A3", col_names = FALSE)

# Leer datos de las importaciones desde la hoja de cálculo de Google todo el contenido de las columnas A, B y C
importaciones <- read_sheet("1XfoW5cfUi0UrURPEved0dc20BBepYQ5V874xkuFz4Hk", range = "A:C", col_names = TRUE)

# Convertir todas las columnas a caracteres
importaciones <- data.frame(lapply(importaciones, as.character), stringsAsFactors = FALSE)

# Eliminar los espacios en blanco antes y después de cada elemento
iSurveyIDs <- trimws(importaciones[,1])
url_gsheets <- trimws(importaciones[,2])
sheet_names <- trimws(importaciones[,3])


# Leer datos de autenticación
username <- as.character(credentials[1,1])
password <- as.character(credentials[2,1])
url <- as.character(credentials[3,1])

# Iniciar sesión en las APIs
options(lime_api = url)
options(lime_username = username)
options(lime_password = password)

# Obtener clave de sesión de la API de limesurvey
get_session_key()

# Llamar a la función con los datos leídos de importaciones.txt
# usando purrr::map2() para hacer un bucle a través de cada conjunto de encuesta/hoja/URL
mapply(write_responses_to_sheet, iSurveyIDs, sheet_names, url_gsheets) 

release_session_key()
