library(googledrive)
library(googlesheets4)
library(tidyverse)
library(httr)
library(jsonlite)
library(base64enc)

session_cache <- new.env(parent = emptyenv())



# Obtén el token de la variable de entorno
gdrive_token <- Sys.getenv("GDRIVE_TOKEN")

# Autenticación
drive_auth(path = gdrive_token)
gs4_auth(path = gdrive_token)


base64_to_df <- function(x) {
  raw_csv <- rawToChar(base64enc::base64decode(x))

  return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE, sep = ";"))
}
get_participants <- function(iSurveyID, iStart, iLimit, bUnused, aAttributes){
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())

  results <- call_limer(method = "list_participants", params = params)
  return(data.frame(results))
}
get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", ...) {
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  dots <- list(...)
  if(length(dots) > 0) params <- append(params,dots)
  # print(params) # uncomment to debug the params

  results <- call_limer(method = "export_responses", params = params)
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
  r <- POST(getOption('lime_api'), content_type_json(),
            body = jsonlite::toJSON(body.json, auto_unbox = TRUE))

  session_key <- as.character(jsonlite::fromJSON(content(r, encoding="utf-8"))$result)
  session_cache$session_key <- session_key
  session_key
}



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




# Definir el ID del archivo y el nombre del archivo temporal
file_id_pass <- "1cBUqmb3XyCD7S9imEZq-A7QAWE5QmH1Wo87xjCnviYM"
temp_pass <- tempfile(fileext = ".txt")

# Descargar el archivo
drive_download(as_id(file_id_pass), path = temp_pass, overwrite = TRUE)

# Leer el archivo
credentials <- readLines(temp_pass)

# Definir el ID del archivo y el nombre del archivo temporal
file_id_importaciones <- "1FkSgv6ZIvsHWazW8bWzdRGYnc4r3sdcMEePWe5ixSYA"
temp_importaciones <- tempfile(fileext = ".txt")

# Descargar el archivo
drive_download(as_id(file_id_importaciones), path = temp_importaciones, overwrite = TRUE)

# Leer el archivo
datos <- readLines(temp_importaciones)

# Leer datos de autentificación
username <- credentials[1]
password <- credentials[2]
url <- credentials[3]
mail <- credentials[4]

# Leer datos de las importaciones
iSurveyIDs <- str_split(datos[1], pattern = ",")[[1]] # Separa las encuestas por comas
sheet_names <- str_split(datos[2], pattern = ",")[[1]] # Separa los nombres de las hojas por comas
url_gsheets <- str_split(datos[3], pattern = ",")[[1]] # Separa las URLs de Google Sheets por comas

# Asegurar que tenemos el mismo número de encuestas, nombres de hojas y URLs de Google Sheets
if (length(iSurveyIDs) != length(sheet_names) | length(iSurveyIDs) != length(url_gsheets)) {
  stop("El número de IDs de encuestas, nombres de hojas y URLs de Google Sheets debe ser el mismo.")
}

# Eliminar los espacios en blanco antes y después de cada elemento
iSurveyIDs <- trimws(iSurveyIDs)
sheet_names <- trimws(sheet_names)
url_gsheets <- trimws(url_gsheets)


# Iniciar sesión en las APIs
options(lime_api = url)
options(lime_username = username)
options(lime_password = password)

# Obtener clave de sesión de la API de limesurvey
get_session_key()

# Función para obtener respuestas y escribir en Google Sheets
write_responses_to_sheet <- function(iSurveyID, sheet_name, url_gsheet) {
  responses <- get_responses(iSurveyID = iSurveyID)
  range_write(responses, ss = url_gsheet, sheet = sheet_name, range = "A2", col_names = FALSE)
}


# Llamar a la función con los datos leídos de importaciones.txt
# usando purrr::map2() para hacer un bucle a través de cada conjunto de encuesta/hoja/URL
purrr::pmap(list(iSurveyIDs, sheet_names, url_gsheets), write_responses_to_sheet)




# Cerrar sesión en la API de limesurvey
release_session_key()




