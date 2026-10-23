# Rellena attribute_7 <submateria> en las encuestas 389243 (Teléfono) y 689133 (resto de canales)
# a partir de atributo7.xlsx (export de participantes con la columna "attribute_7 <submateria>").
#
# - Cruza por token: cada participante se actualiza en la encuesta donde está su token.
# - Solo escribe si el valor actual en LimeSurvey es distinto (se puede relanzar sin repetir trabajo).
# - Primero los participantes con encuesta completada (los que usa el dashboard), luego el resto.
#
# Antes: crear attribute_7 con descripción "submateria" en las DOS encuestas.
# Uso: APLICAR <- FALSE solo enseña el resumen (dry-run); TRUE escribe en LimeSurvey.

packages <- c("httr", "jsonlite", "dotenv", "readxl")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    require(pkg, character.only = TRUE)
  }
}

dotenv::load_dot_env(".env")

options(lime_api = Sys.getenv("LIME_API"))
options(lime_username = Sys.getenv("LIME_USERNAME"))
options(lime_password = Sys.getenv("LIME_PASSWORD"))

# --- PARÁMETROS ---
APLICAR   <- TRUE                                  # TRUE para escribir en LimeSurvey
ENCUESTAS <- c("389243" = "Teléfono", "689133" = "Resto de canales")
ATRIBUTO  <- "attribute_7"
XLSX      <- "atributo7.xlsx"
SOLO_COMPLETADOS <- FALSE                           # TRUE = solo quienes ya han completado (más rápido)
LOG_CSV   <- "log_attribute_7.csv"                  # *.csv está en .gitignore

# --- SESIÓN (igual que en tus scripts) ---
session_cache <- new.env(parent = emptyenv())

get_session_key <- function(username = getOption("lime_username"),
                            password = getOption("lime_password")) {
  body.json <- list(method = "get_session_key", id = " ",
                    params = list(username = username, password = password))
  r <- POST(getOption("lime_api"), content_type_json(),
            body = jsonlite::toJSON(body.json, auto_unbox = TRUE))
  res <- jsonlite::fromJSON(content(r, as = "text", encoding = "utf-8"))$result
  if (is.list(res)) stop("Login fallido: ", res$status)
  session_cache$session_key <- as.character(res)
  session_cache$session_key
}

call_limer <- function(method, params = list(), ...) {
  if (!exists("session_key", envir = session_cache)) stop("Run get_session_key() first.")
  body.json <- list(method = method, id = " ",
                    params = c(list(sSessionKey = session_cache$session_key), params))
  r <- httr::POST(getOption("lime_api"), httr::content_type_json(),
                  body = jsonlite::toJSON(body.json, auto_unbox = TRUE), ...)
  jsonlite::fromJSON(httr::content(r, as = "text", encoding = "utf-8"),
                     simplifyVector = FALSE)$result
}

release_session_key <- function() call_limer(method = "release_session_key")

nz <- function(x) if (is.null(x)) "" else as.character(x)

participantes <- function(sid) {
  res <- call_limer("list_participants", list(
    iSurveyID = sid, iStart = 0, iLimit = 1000000, bUnused = FALSE,
    aAttributes = list("completed", ATRIBUTO)))
  if (!is.null(names(res)) && !is.null(res$status)) stop(sid, ": ", res$status)
  data.frame(
    sid       = sid,
    tid       = vapply(res, function(x) nz(x$tid), ""),
    token     = vapply(res, function(x) nz(x$token), ""),
    completed = vapply(res, function(x) nz(x$completed), ""),
    actual    = vapply(res, function(x) nz(x[[ATRIBUTO]]), ""),
    stringsAsFactors = FALSE)
}

set_atributo <- function(sid, tid, valor) {
  call_limer("set_participant_properties", list(
    iSurveyID = sid,
    aTokenQueryProperties = tid,
    aTokenData = setNames(list(valor), ATRIBUTO)))
}

# --- DATOS ---
x <- read_excel(XLSX, col_types = "text")
col <- function(patron) {
  i <- grep(patron, names(x))
  if (length(i) != 1) stop("No encuentro una columna única que empiece por ", patron)
  x[[i]]
}
excel <- data.frame(token = trimws(x$token),
                    canal = trimws(col("^attribute_2")),
                    valor = trimws(col(paste0("^", ATRIBUTO))),
                    stringsAsFactors = FALSE)
excel <- excel[!is.na(excel$token) & excel$token != "", ]
stopifnot(!anyDuplicated(excel$token))
cat("Excel:", nrow(excel), "participantes |", sum(is.na(excel$valor) | excel$valor == ""),
    "sin submateria (se ignoran)\n")
excel <- excel[!is.na(excel$valor) & excel$valor != "", ]

get_session_key()
ls_part <- do.call(rbind, lapply(as.integer(names(ENCUESTAS)), participantes))
cat("LimeSurvey:", paste(names(ENCUESTAS), table(factor(ls_part$sid, names(ENCUESTAS))),
                         sep = " = ", collapse = " | "), "\n")

d <- merge(excel, ls_part, by = "token", all.x = TRUE)

# Controles
sin_ls <- d[is.na(d$sid), ]
d <- d[!is.na(d$sid), ]
canal_raro <- d[(d$canal == "Teléfono") != (d$sid == 389243L), ]
cat("No encontrados en ninguna encuesta:", nrow(sin_ls), "\n")
cat("Canal que no cuadra con la encuesta (se actualizan igual, en la encuesta del token):",
    nrow(canal_raro), "\n")

ya_ok <- sum(d$actual == d$valor)
d <- d[d$actual != d$valor, ]
if (SOLO_COMPLETADOS) d <- d[d$completed != "N", ]
d <- d[order(d$completed == "N", d$sid, as.integer(d$tid)), ]   # completados primero
cat("Ya correctos:", ya_ok,
    "| Pendientes de escribir:", nrow(d),
    "(completados:", sum(d$completed != "N"), ")",
    "| Modo:", if (APLICAR) "APLICAR" else "DRY-RUN", "\n")
print(table(encuesta = ENCUESTAS[as.character(d$sid)]))

# --- EJECUTAR ---
if (APLICAR && nrow(d)) {
  d$resultado <- NA_character_
  t0 <- Sys.time()
  for (i in seq_len(nrow(d))) {
    res <- tryCatch(set_atributo(d$sid[i], as.integer(d$tid[i]), d$valor[i]),
                    error = function(e) list(status = paste("error R:", conditionMessage(e))))

    # Si la sesión ha caducado, renueva y reintenta una vez
    if (!is.null(res$status) && grepl("session", res$status, ignore.case = TRUE)) {
      get_session_key()
      res <- tryCatch(set_atributo(d$sid[i], as.integer(d$tid[i]), d$valor[i]),
                      error = function(e) list(status = paste("error R:", conditionMessage(e))))
    }

    d$resultado[i] <- if (!is.null(res$status)) {
      res$status
    } else if (!identical(nz(res$token), d$token[i])) {
      paste("token distinto:", nz(res$token))
    } else if (!identical(nz(res[[ATRIBUTO]]), d$valor[i])) {
      paste("valor no guardado:", nz(res[[ATRIBUTO]]))
    } else {
      "OK"
    }

    if (i %% 500 == 0 || i == nrow(d)) {
      seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      cat(sprintf("  %d/%d (%.0f s, faltan ~%.0f min)\n", i, nrow(d), seg,
                  seg / i * (nrow(d) - i) / 60))
      # Log parcial: si se corta, al relanzar solo hace lo que falte
      write.csv(d[seq_len(i), c("sid", "tid", "token", "completed", "valor", "resultado")],
                LOG_CSV, row.names = FALSE, fileEncoding = "UTF-8")
    }
  }

  cat("OK:", sum(d$resultado == "OK"),
      "| Errores:", sum(d$resultado != "OK"), "| Log:", LOG_CSV, "\n")
  print(head(d[d$resultado != "OK", c("sid", "tid", "valor", "resultado")], 20))
}

release_session_key()
