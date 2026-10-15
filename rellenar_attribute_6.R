# Rellena attribute_6 <identificador_muestra> de la encuesta 389243
# con números correlativos desde 50000, en orden de tid.
#
# Uso: ajusta APLICAR y ejecuta. Con APLICAR <- FALSE solo hace login
# y enseña lo que haría (dry-run).

packages <- c("httr", "jsonlite", "dotenv")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    require(pkg, character.only = TRUE)
  }
}

library(httr)
library(jsonlite)
library(dotenv)

dotenv::load_dot_env(".env")

options(lime_api = Sys.getenv("LIME_API"))
options(lime_username = Sys.getenv("LIME_USERNAME"))
options(lime_password = Sys.getenv("LIME_PASSWORD"))

# --- PARÁMETROS ---
APLICAR    <- FALSE                      # TRUE para escribir en LimeSurvey
SURVEY_ID  <- 999901
ATRIBUTO   <- "attribute_6"
INICIO     <- 50000
CSV_TOKENS <- "tokens_999901.csv"      # export de participantes
EXCLUIR    <- character(0)               # p.ej. c("prueba") para no numerar ese token
LOG_CSV    <- "log_attribute_6.csv"

# --- SESIÓN (igual que en tus scripts) ---
session_cache <- new.env(parent = emptyenv())

get_session_key <- function(username = getOption("lime_username"),
                            password = getOption("lime_password")) {
  body.json <- list(
    method = "get_session_key",
    id = " ",
    params = list(username = username, password = password)
  )
  r <- POST(getOption("lime_api"), content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE)
  )
  res <- jsonlite::fromJSON(content(r, as = "text", encoding = "utf-8"))$result
  if (is.list(res)) stop("Login fallido: ", res$status)
  session_cache$session_key <- as.character(res)
  session_cache$session_key
}

call_limer <- function(method, params = list(), ...) {
  if (!exists("session_key", envir = session_cache)) {
    stop("You need to get a session key first. Run get_session_key().")
  }
  body.json <- list(
    method = method,
    id = " ",
    params = c(list(sSessionKey = session_cache$session_key), params)
  )
  r <- httr::POST(getOption("lime_api"), httr::content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE), ...
  )
  jsonlite::fromJSON(httr::content(r, as = "text", encoding = "utf-8"),
                     simplifyVector = FALSE)$result
}

release_session_key <- function() call_limer(method = "release_session_key")

set_atributo <- function(tid, valor) {
  call_limer("set_participant_properties", list(
    iSurveyID = SURVEY_ID,
    aTokenQueryProperties = tid,
    aTokenData = setNames(list(valor), ATRIBUTO)
  ))
}

# --- DATOS ---
part <- read.csv(CSV_TOKENS, colClasses = "character", check.names = FALSE,
                 fileEncoding = "UTF-8-BOM", na.strings = character(0))
names(part) <- trimws(names(part))
part <- part[order(as.integer(part$tid)), c("tid", "token")]
part <- part[!part$token %in% EXCLUIR, ]

stopifnot(!anyDuplicated(part$tid), !anyDuplicated(part$token), all(part$token != ""))

part$valor <- as.character(seq(INICIO, length.out = nrow(part)))
cat(nrow(part), "participantes:", part$valor[1], "a", part$valor[nrow(part)],
    "| Modo:", if (APLICAR) "APLICAR" else "DRY-RUN", "\n")
print(head(part, 5))

# --- EJECUTAR ---
get_session_key()

part$resultado <- NA_character_

if (APLICAR) {
  t0 <- Sys.time()
  for (i in seq_len(nrow(part))) {
    res <- tryCatch(set_atributo(as.integer(part$tid[i]), part$valor[i]),
                    error = function(e) list(status = paste("error R:", conditionMessage(e))))

    # Si la sesión ha caducado, renueva y reintenta una vez
    if (!is.null(res$status) && grepl("session", res$status, ignore.case = TRUE)) {
      get_session_key()
      res <- set_atributo(as.integer(part$tid[i]), part$valor[i])
    }

    part$resultado[i] <- if (!is.null(res$status)) {
      res$status
    } else if (!identical(res$token, part$token[i])) {
      paste("token distinto:", res$token)
    } else if (!identical(as.character(res[[ATRIBUTO]]), part$valor[i])) {
      paste("valor no guardado:", res[[ATRIBUTO]])
    } else {
      "OK"
    }

    if (i %% 500 == 0) {
      cat(sprintf("  %d/%d (%.0f s)\n", i, nrow(part),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
  }

  write.csv(part, LOG_CSV, row.names = FALSE, fileEncoding = "UTF-8")
  cat("OK:", sum(part$resultado == "OK"),
      "| Errores:", sum(part$resultado != "OK"), "| Log:", LOG_CSV, "\n")
  print(head(part[part$resultado != "OK", ], 20))
}

release_session_key()
