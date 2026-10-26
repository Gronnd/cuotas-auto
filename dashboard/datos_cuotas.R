# Extrae de LimeSurvey lo mínimo para el dashboard de cuotas (SYRCI):
# modo, fecha, materia, submateria, tipo de solicitud, canal de entrada y respuestas a PART
# de cada encuesta completada.
# NO guarda datos personales (ni token, ni teléfono, ni email).
#
# Salida: dashboard/_datos/respuestas.rds  (lo lee index.qmd)
#
# Credenciales, igual que envio_exfe.R:
#   Actions -> GDRIVE_TOKEN (JSON cuenta de servicio) + CREDENCIALES_URL (A1 usuario, A2 contraseña, A3 URL API)
#   Local   -> .env con LIME_USERNAME / LIME_PASSWORD / LIME_API
#
# OJO: el repo es público y los logs de Actions también. No imprimir nada de los datos.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

if (file.exists(".env")) dotenv::load_dot_env(".env")

# --- PARÁMETROS ---
ENCUESTAS    <- c("389243" = "Teléfono", "689133" = "Online")
PREGUNTA     <- "PART"         # pregunta de aceptación (multirrespuesta)
CODIGO_OK    <- "SI"           # para las cuotas solo cuentan quienes marcan PART[SI]
EXCLUIR_TOKEN <- "^TEST"       # tokens de prueba (solo al inicio: hay tokens reales con "test" dentro por azar)
EXCLUIR_TEST  <- "test"        # casos de prueba: "test" (sin distinguir mayúsculas) en materia, submateria,
                               # tipo o canal (TESTcanal, test_solicitud_{COUNTER}...)
ESTADO       <- "complete"
TZ           <- "Europe/Madrid"
SALIDA       <- file.path("dashboard", "_datos", "respuestas.rds")

# Atributos de participante con materia y submateria. Se detectan por la descripción
# del atributo en cada encuesta; si no se encuentran, se usan estos (o los de las variables de entorno).
ATTR_MATERIA_DEF    <- Sys.getenv("ATTR_MATERIA", "attribute_3")
ATTR_SUBMATERIA_DEF <- Sys.getenv("ATTR_SUBMATERIA", "attribute_7")
ATTR_TIPO_DEF       <- Sys.getenv("ATTR_TIPO", "attribute_1")      # Tipo de solicitud
ATTR_CANAL_DEF      <- Sys.getenv("ATTR_CANAL", "attribute_2")     # Tipo canal de entrada

# --- CREDENCIALES ---
if (Sys.getenv("CREDENCIALES_URL") != "") {
  suppressPackageStartupMessages(library(googlesheets4))
  options(rlang_interactive = FALSE, gargle_oauth_cache = FALSE, gargle_quiet = TRUE)
  key_file <- Sys.getenv("GDRIVE_KEY_FILE")
  if (key_file == "" && file.exists(".secrets/sa.json")) key_file <- ".secrets/sa.json"
  if (key_file == "") {
    if (Sys.getenv("GDRIVE_TOKEN") == "") stop("Falta GDRIVE_TOKEN (Actions) o GDRIVE_KEY_FILE (local)")
    key_file <- tempfile(fileext = ".json")
    writeLines(Sys.getenv("GDRIVE_TOKEN"), key_file)
  }
  gs4_auth(path = key_file)
  cred <- range_read(Sys.getenv("CREDENCIALES_URL"), range = "A1:A3", col_names = FALSE,
                     col_types = "c", .name_repair = "minimal")
  options(lime_username = trimws(cred[[1]][1]),
          lime_password = trimws(cred[[1]][2]),
          lime_api      = trimws(cred[[1]][3]))
} else {
  options(lime_username = Sys.getenv("LIME_USERNAME"),
          lime_password = Sys.getenv("LIME_PASSWORD"),
          lime_api      = Sys.getenv("LIME_API"))
}
if (getOption("lime_api") %in% c("", NA)) stop("Faltan las credenciales de LimeSurvey")

# --- API RemoteControl ---
session_cache <- new.env(parent = emptyenv())

get_session_key <- function() {
  body <- list(method = "get_session_key", id = " ",
               params = list(username = getOption("lime_username"),
                             password = getOption("lime_password")))
  r <- POST(getOption("lime_api"), content_type_json(),
            body = toJSON(body, auto_unbox = TRUE))
  res <- fromJSON(content(r, as = "text", encoding = "utf-8"))$result
  if (is.list(res)) stop("Login fallido en LimeSurvey")
  session_cache$session_key <- as.character(res)
}

call_limer <- function(method, params = list()) {
  body <- list(method = method, id = " ",
               params = c(list(sSessionKey = session_cache$session_key), params))
  r <- POST(getOption("lime_api"), content_type_json(),
            body = toJSON(body, auto_unbox = TRUE))
  fromJSON(content(r, as = "text", encoding = "utf-8"), simplifyVector = FALSE)$result
}

nz <- function(x) if (is.null(x)) "" else as.character(x)
marcado <- function(x) x %in% c("Y", "Sí")

get_respuestas <- function(sid) {
  res <- call_limer("export_responses", list(
    iSurveyID = sid, sDocumentType = "csv", sLanguageCode = "",
    sCompletionStatus = ESTADO, sHeadingType = "code", sResponseType = "short"))
  if (is.list(res)) return(NULL)                       # sin respuestas
  txt <- rawToChar(base64_dec(res))
  Encoding(txt) <- "UTF-8"
  txt <- sub("^﻿", "", txt)
  cab <- sub("\r?\n.*", "", txt)
  sep <- if (lengths(gregexpr(";", cab)) > lengths(gregexpr(",", cab))) ";" else ","
  read.csv(text = txt, sep = sep, colClasses = "character", check.names = FALSE,
           na.strings = character(0))
}

# Busca qué attribute_N es materia y cuál submateria según su descripción
detectar_atributos <- function(sid) {
  props <- tryCatch(call_limer("get_survey_properties",
                               list(iSurveyID = sid, aSurveySettings = list("attributedescriptions"))),
                    error = function(e) NULL)
  desc <- props$attributedescriptions
  if (is.character(desc)) desc <- tryCatch(fromJSON(desc, simplifyVector = FALSE), error = function(e) NULL)
  out <- c(materia = ATTR_MATERIA_DEF, submateria = ATTR_SUBMATERIA_DEF,
           tipo = ATTR_TIPO_DEF, canal = ATTR_CANAL_DEF)
  if (is.list(desc) && length(desc)) {
    d <- vapply(desc, function(x) tolower(nz(if (is.list(x)) x$description else x)), "")
    sub_i <- grep("sub.?materia", d)
    mat_i <- setdiff(grep("materia", d), sub_i)
    if (length(mat_i)) out["materia"]    <- names(d)[mat_i[1]]
    if (length(sub_i)) out["submateria"] <- names(d)[sub_i[1]]
    tipo_i  <- grep("tipo de solicitud", d)
    canal_i <- grep("canal", d)
    if (length(tipo_i))  out["tipo"]  <- names(d)[tipo_i[1]]
    if (length(canal_i)) out["canal"] <- names(d)[canal_i[1]]
  }
  out
}

get_participantes <- function(sid, attrs) {
  res <- call_limer("list_participants", list(
    iSurveyID = sid, iStart = 0, iLimit = 1000000, bUnused = FALSE,
    aAttributes = as.list(unname(attrs))))
  if (!is.null(names(res)) && !is.null(res$status)) return(NULL)
  data.frame(
    token      = vapply(res, function(x) nz(x$token), ""),
    materia    = vapply(res, function(x) nz(x[[attrs["materia"]]]), ""),
    submateria = vapply(res, function(x) nz(x[[attrs["submateria"]]]), ""),
    tipo       = vapply(res, function(x) nz(x[[attrs["tipo"]]]), ""),
    canal      = vapply(res, function(x) nz(x[[attrs["canal"]]]), ""),
    stringsAsFactors = FALSE)
}

a_madrid <- function(x) {
  f <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  as.POSIXct(format(f, "%Y-%m-%d %H:%M:%S", tz = TZ), tz = TZ)
}

datos_encuesta <- function(sid) {
  modo <- unname(ENCUESTAS[as.character(sid)])
  r <- get_respuestas(sid)
  if (is.null(r) || !nrow(r)) { cat(modo, ": sin respuestas\n"); return(NULL) }
  r <- r[!grepl(EXCLUIR_TOKEN, r$token), ]
  if (!nrow(r)) { cat(modo, ": sin respuestas válidas\n"); return(NULL) }

  cols_part <- grep(paste0("^", PREGUNTA, "\\["), names(r), value = TRUE)
  col_ok <- paste0(PREGUNTA, "[", CODIGO_OK, "]")
  if (!col_ok %in% cols_part) stop(modo, ": no existe la columna ", col_ok)

  attrs <- detectar_atributos(sid)
  cat(modo, ":", paste(names(attrs), attrs, sep = " = ", collapse = " | "), "\n")
  p <- get_participantes(sid, attrs)
  if (is.null(p)) p <- data.frame(token = character(0), materia = character(0), submateria = character(0),
                                  tipo = character(0), canal = character(0))
  d <- merge(r[, c("token", "submitdate", intersect("materia", names(r)), cols_part)], p,
             by = "token", all.x = TRUE, suffixes = c("_resp", ""))
  # Si el participante no trae materia, se usa la de la respuesta (pregunta 'materia')
  if ("materia_resp" %in% names(d)) {
    vacia <- is.na(d$materia) | d$materia == ""
    d$materia[vacia] <- d$materia_resp[vacia]
  }
  out <- data.frame(modo = modo, fecha = a_madrid(d$submitdate),
                    materia = trimws(d$materia), submateria = trimws(d$submateria),
                    tipo = trimws(d$tipo), canal = trimws(d$canal), stringsAsFactors = FALSE)
  # Una columna lógica por opción de PART (SI, EX26, EXFE, EJEPdD...)
  for (cc in cols_part) out[[sub(".*\\[(.*)\\]", "\\1", cc)]] <- marcado(d[[cc]])

  # Fuera los casos de prueba marcados en los atributos del participante
  es_test <- Reduce(`|`, lapply(out[c("materia", "submateria", "tipo", "canal")],
                                function(x) grepl(EXCLUIR_TEST, x, ignore.case = TRUE)))
  if (any(es_test)) cat(modo, ": ", sum(es_test), " casos de prueba excluidos\n", sep = "")
  out <- out[!es_test, ]
  cat(modo, ": ", nrow(out), " completas, ", sum(out[[CODIGO_OK]]), " con ", col_ok, "\n", sep = "")
  out
}

# Une data.frames con columnas distintas (las opciones de PART pueden variar entre encuestas)
unir <- function(lista) {
  lista <- Filter(Negate(is.null), lista)
  if (!length(lista)) return(NULL)
  cols <- unique(unlist(lapply(lista, names)))
  do.call(rbind, lapply(lista, function(x) {
    for (cc in setdiff(cols, names(x))) x[[cc]] <- FALSE
    x[, cols]
  }))
}

# --- EJECUTAR ---
get_session_key()
resp <- unir(lapply(as.integer(names(ENCUESTAS)), datos_encuesta))
invisible(call_limer("release_session_key"))

# resp: todas las completas (para PART). Las cuotas usan solo las filas con SI = TRUE.
if (is.null(resp)) resp <- data.frame(modo = character(0), fecha = as.POSIXct(character(0)),
                                      materia = character(0), submateria = character(0),
                                      tipo = character(0), canal = character(0), SI = logical(0))
dir.create(dirname(SALIDA), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(resp = resp, actualizado = Sys.time()), SALIDA)
cat("Guardado", SALIDA, "\n")
