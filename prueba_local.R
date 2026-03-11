library(googlesheets4)
library(httr)
library(jsonlite)
library(base64enc)
library(dotenv)

# Cargar variables de entorno desde .env (solo en local)
dotenv::load_dot_env(".env")

# --- AUTENTICACIÓN GOOGLE SHEETS (local: usa sa.json directamente) ---
gs4_auth(path = ".secrets/sa.json")


# --- CREDENCIALES LIMESURVEY (desde .env) ---
options(lime_api = Sys.getenv("LIME_API"))
options(lime_username = Sys.getenv("LIME_USERNAME"))
options(lime_password = Sys.getenv("LIME_PASSWORD"))
options(importaciones_url = Sys.getenv("IMPORTACIONES_URL"))

# --- ENTORNO DE SESIÓN ---
session_cache <- new.env(parent = emptyenv())

# --- FUNCIONES ---

# Versión mejorada del remoto: maneja encoding UTF-8 y latin1
base64_to_df <- function(x) {
  raw_bytes <- base64enc::base64decode(x)
  tmp <- tempfile(fileext = ".csv")
  writeBin(raw_bytes, tmp)

  result <- tryCatch(
    {
      # Intentar UTF-8 suprimiendo warnings
      withCallingHandlers(
        read.csv(tmp, stringsAsFactors = FALSE, sep = ";", fileEncoding = "UTF-8"),
        warning = function(w) invokeRestart("muffleWarning")
      )
    },
    error = function(e) {
      # Si falla del todo, probar latin1
      read.csv(tmp, stringsAsFactors = FALSE, sep = ";", fileEncoding = "latin1")
    }
  )

  unlink(tmp)
  return(result)
}

get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", ...) {
  params <- as.list(environment())
  dots <- list(...)
  if (length(dots) > 0) params <- append(params, dots)
  results <- call_limer(method = "export_responses", params = params)
  return(base64_to_df(unlist(results)))
}

get_session_key <- function(username = getOption("lime_username"),
                            password = getOption("lime_password")) {
  body.json <- list(
    method = "get_session_key",
    id = " ",
    params = list(
      username = username,
      password = password
    )
  )
  r <- POST(getOption("lime_api"), content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE)
  )
  session_key <- as.character(jsonlite::fromJSON(content(r, as = "text", encoding = "utf-8"))$result)
  session_cache$session_key <- session_key
  session_key
}

call_limer <- function(method, params = list(), ...) {
  if (!is.list(params)) stop("params must be a list.")
  if (!exists("session_key", envir = session_cache)) {
    stop("You need to get a session key first. Run get_session_key().")
  }
  key.list <- list(sSessionKey = session_cache$session_key)
  params.full <- c(key.list, params)
  body.json <- list(
    method = method,
    id = " ",
    params = params.full
  )
  r <- httr::POST(getOption("lime_api"), httr::content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE), ...
  )
  return(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "utf-8"))$result)
}

write_responses_to_sheet <- function(iSurveyID, sheet_name, url_gsheet) {
  responses <- tryCatch(
    get_responses(iSurveyID = iSurveyID),
    error = function(e) {
      cat("⚠ Encuesta", iSurveyID, "(", sheet_name, ") sin respuestas o error:", e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(responses) || nrow(responses) == 0) {
    cat("⚠ Encuesta", iSurveyID, "(", sheet_name, ") vacía, omitiendo.\n")
    return(invisible(NULL))
  }

  range_write(responses, ss = url_gsheet, sheet = sheet_name, range = "A2", col_names = FALSE)
}

release_session_key <- function() {
  call_limer(method = "release_session_key")
}

# --- LEER DATOS DESDE GOOGLE SHEETS (igual que el remoto) ---
importaciones <- read_sheet(Sys.getenv("IMPORTACIONES_URL"), range = "A:C", col_names = TRUE)

importaciones <- data.frame(lapply(importaciones, as.character), stringsAsFactors = FALSE)

iSurveyIDs <- trimws(importaciones[, 1])
url_gsheets <- trimws(importaciones[, 2])
sheet_names <- trimws(importaciones[, 3])

# --- EJECUTAR ---
get_session_key()

mapply(write_responses_to_sheet, iSurveyIDs, sheet_names, url_gsheets)

release_session_key()


# La sesión puede haber caducado, obtener una nueva
get_session_key()
