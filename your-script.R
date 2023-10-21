install.packages("remotes")
library(remotes, quietly = TRUE)
install_github("cloudyr/limer")
install.packages("googledrive")

library(googledrive, quietly = TRUE)
library(limer, quietly = TRUE)
library(googlesheets4, quietly = TRUE)
library(tidyverse, quietly = TRUE)



# Obtener acceso a google sheets y google drive
credenciales_json <- Sys.getenv("GDRIVE_CREDENTIALS")
cat(credenciales_json, file = "gdrive-auth-file.json")
googledrive::drive_auth(path = "gdrive-auth-file.json")

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

# Función para imprimir URLs como enlaces HTML
print_sheet_links <- function(url_gsheets) {
  unique_urls <- unique(url_gsheets)
  cat(paste(unique_urls, collapse = "\n"), sep = "\n")
}

# Llamar a la función con los datos leídos de importaciones.txt
# usando purrr::map2() para hacer un bucle a través de cada conjunto de encuesta/hoja/URL
purrr::pmap(list(iSurveyIDs, sheet_names, url_gsheets), write_responses_to_sheet)

# Imprimir URLs
print_sheet_links(url_gsheets)


# Cerrar sesión en la API de limesurvey
release_session_key()

# Limpiar el entorno de trabajo
rm(list = ls())
