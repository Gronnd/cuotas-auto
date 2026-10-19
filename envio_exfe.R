# Listado de personas que marcan EXFE en PART (encuestas 389243 CATI y 689133 CAWI)
# y envío de UN correo con el listado en Excel adjunto.
#
# Configuración en la PRIMERA pestaña de la hoja de Google "madrid-correos" (A1:Z10):
#   - filas de destinatarios: en la columna A la etiqueta "para", "cc" o "cco";
#     los correos en las celdas siguientes o en una sola separados por , o ;
#     (una fila con correos sin etiqueta reconocida cuenta como "para")
#   - una celda con "diario" o "total":
#       diario -> casos desde el último día de envío hasta ayer (hora de Madrid):
#                 de martes a viernes, los de ayer; el lunes, los de viernes, sábado y domingo
#       total  -> todos los casos hasta ahora
#
# Uso: con APLICAR <- FALSE genera el Excel y enseña el resumen, pero no envía.

packages <- c("httr", "jsonlite", "dotenv", "curl", "writexl", "googlesheets4", "googledrive")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    require(pkg, character.only = TRUE)
  }
}

library(httr)
library(jsonlite)
library(googlesheets4)

if (file.exists(".env")) dotenv::load_dot_env(".env")


# --- PARÁMETROS ---
APLICAR         <- toupper(Sys.getenv("APLICAR", "FALSE")) == "TRUE"   # envía solo si APLICAR=TRUE (Actions o .env)
ENCUESTAS       <- c("389243" = "CATI", "689133" = "CAWI")
PREGUNTA        <- "PART"
CODIGO          <- "EXFE"
OTROS_CODIGOS   <- c("SI", "EX26", "EJEPdD")            # para anotar si marcó más de una opción
ESTADO          <- "complete"                           # "complete", "incomplete" o "all"
EXCLUIR_TEST    <- "^TEST"                              # tokens o códigos de prueba
FECHAS_UTC      <- TRUE                                 # LimeSurvey 7 exporta las fechas en UTC
TZ              <- "Europe/Madrid"
ENVIAR_SI_VACIO <- FALSE                                # en modo diario, ¿enviar aunque no haya casos?
DIAS_ENVIO      <- 1:5                                  # días que se envía en modo diario (1 = lunes ... 7 = domingo)

HOJA_CONFIG     <- "madrid-correos"                     # nombre de la hoja en Drive
HOJA_CONFIG_URL <- Sys.getenv("MADRID_CORREOS_URL")     # si está definida, se usa en vez del nombre
PESTANA_LOG     <- "log"                                # pestaña donde se apunta cada envío (se crea sola)
# La configuración (correos y diario/total) se lee siempre de la PRIMERA pestaña.
# La cuenta de servicio necesita permiso de EDITOR en madrid-correos para escribir el log.

SMTP_SERVER     <- "smtps://mail.edesga.com:465"
SMTP_USER       <- Sys.getenv("SMTP_USER")              # jorge.acuna@edesga.com
SMTP_PASS       <- Sys.getenv("SMTP_PASS")
REMITENTE       <- "EDESGA <jorge.acuna@edesga.com>"    # debe ser la cuenta autenticada
# Destinatarios (Para / CC / CCO) y modo (diario / total): en la hoja madrid-correos

# --- TEXTO DEL CORREO (edita aquí) ---
# Marcadores que se sustituyen solos:
#   {fecha}  -> diario: "16/10/2026" o, los lunes, "16/10/2026 al 18/10/2026" | total: fecha de hoy
#   {total}  -> número de casos | {cati} / {cawi} -> casos por modo
# El cuerpo es HTML: <p>...</p> = párrafo, <br> = salto de línea, <b>...</b> = negrita.
ASUNTO_DIARIO <- "SYRCI - EXFE - reporte diario del {fecha}"
ASUNTO_TOTAL  <- "SYRCI - EXFE - acumulado a {fecha}"

CUERPO_DIARIO <- "
<p>Buenos días:</p>
<p>Adjuntamos el listado de personas que, en la encuesta del Sistema de Sugerencias y Reclamaciones,
han indicado que no desean participar ni recibir futuras encuestas. Incluye los casos del {fecha}.</p>
<p>Total: {total} (CATI: {cati}, CAWI: {cawi}).</p>
<p>Un saludo,<br>EDESGA</p>
"

CUERPO_TOTAL <- "
<p>Buenos días:</p>
<p>Adjuntamos el listado de personas que, en la encuesta del Sistema de Sugerencias y Reclamaciones,
han indicado que no desean participar ni recibir futuras encuestas. Incluye todos los casos hasta el {fecha}.</p>
<p>Total: {total} (CATI: {cati}, CAWI: {cawi}).</p>
<p>Un saludo,<br>EDESGA</p>
"

rellenar <- function(plantilla, valores) {
  for (k in names(valores)) plantilla <- gsub(paste0("{", k, "}"), valores[[k]], plantilla, fixed = TRUE)
  plantilla
}

# --- AUTENTICACIÓN GOOGLE CON CUENTA DE SERVICIO (sin navegador) ---
# En local: GDRIVE_KEY_FILE = ruta al .json de la cuenta de servicio (por defecto .secrets/sa.json)
# En GitHub Actions: GDRIVE_TOKEN = contenido del .json (secret)
options(rlang_interactive = FALSE, gargle_oauth_cache = FALSE)  # error en vez de abrir el navegador

temp_key_file <- Sys.getenv("GDRIVE_KEY_FILE")
if (temp_key_file == "" && file.exists(".secrets/sa.json")) temp_key_file <- ".secrets/sa.json"
if (temp_key_file == "") {
  service_account_json_content <- Sys.getenv("GDRIVE_TOKEN")
  if (service_account_json_content == "") stop("Define GDRIVE_KEY_FILE (local) o GDRIVE_TOKEN (Actions).")
  temp_key_file <- tempfile(fileext = ".json")
  writeLines(service_account_json_content, temp_key_file)   # tempdir() se borra al cerrar R
}
cuenta <- tryCatch(jsonlite::fromJSON(temp_key_file),
                   error = function(e) stop("El JSON de la cuenta de servicio no es válido: ", conditionMessage(e)))
if (!identical(cuenta$type, "service_account")) stop("El JSON no es de una cuenta de servicio")
cat("Cuenta de servicio:", cuenta$client_email, "\n")

gs4_auth(path = temp_key_file)
googledrive::drive_auth(path = temp_key_file)

# --- CREDENCIALES LIMESURVEY ---
# En Actions: hoja de credenciales (secret CREDENCIALES_URL), igual que el script de Sheets:
#   A1 = usuario, A2 = contraseña, A3 = URL del remotecontrol
# En local: LIME_USERNAME / LIME_PASSWORD / LIME_API en el .env
if (Sys.getenv("CREDENCIALES_URL") != "") {
  credentials <- range_read(Sys.getenv("CREDENCIALES_URL"), range = "A1:A3",
                            col_names = FALSE, col_types = "c", .name_repair = "minimal")
  options(lime_username = trimws(credentials[[1]][1]),
          lime_password = trimws(credentials[[1]][2]),
          lime_api      = trimws(credentials[[1]][3]))
} else {
  options(lime_username = Sys.getenv("LIME_USERNAME"),
          lime_password = Sys.getenv("LIME_PASSWORD"),
          lime_api      = Sys.getenv("LIME_API"))
}
if (getOption("lime_api") %in% c("", NA)) stop("Faltan las credenciales de LimeSurvey (CREDENCIALES_URL o LIME_*)")

# --- CONFIGURACIÓN DESDE LA HOJA ---
leer_config <- function() {
  ss <- if (HOJA_CONFIG_URL != "") HOJA_CONFIG_URL else {
    f <- googledrive::drive_get(HOJA_CONFIG)
    if (nrow(f) != 1) stop("Se esperaba 1 hoja llamada '", HOJA_CONFIG, "' y hay ", nrow(f),
                           ". ¿Está compartida con la cuenta de servicio?")
    f$id
  }
  celdas <- range_read(ss, sheet = 1, range = "A1:Z10", col_names = FALSE, col_types = "c",
                       .name_repair = "minimal")
  trocear <- function(x) {
    v <- trimws(unlist(strsplit(na.omit(as.character(x)), "[,;[:space:]]+")))
    v[v != ""]
  }
  es_email <- function(v) grepl("^[^@]+@[^@]+\\.[^@]+$", v)

  # Cada fila: la etiqueta de la columna A decide el tipo (para / cc / cco).
  # Una fila con correos y sin etiqueta reconocida cuenta como "para".
  dest <- list(para = character(0), cc = character(0), cco = character(0))
  for (i in seq_len(nrow(celdas))) {
    fila <- unlist(celdas[i, ], use.names = FALSE)
    etiqueta <- if (is.na(fila[1])) "" else tolower(trimws(chartr("áéíóú", "aeiou", fila[1])))
    tipo <- if (etiqueta %in% c("cco", "bcc")) "cco"
            else if (etiqueta == "cc") "cc"
            else "para"
    correos <- tolower(Filter(es_email, trocear(fila)))
    dest[[tipo]] <- c(dest[[tipo]], correos)
  }
  dest <- lapply(dest, unique)
  dest$cc  <- setdiff(dest$cc, dest$para)
  dest$cco <- setdiff(dest$cco, c(dest$para, dest$cc))

  modo <- tolower(grep("^(diario|total)$", trocear(unlist(celdas)), value = TRUE, ignore.case = TRUE))

  if (!length(unlist(dest))) stop("No hay direcciones de correo en '", HOJA_CONFIG, "'")
  if (length(unique(modo)) != 1) stop("Pon 'diario' o 'total' (solo uno) en '", HOJA_CONFIG, "'")
  c(list(ss = ss, modo = modo[1]), dest)
}

texto_destinatarios <- function(cfg) {
  partes <- c(if (length(cfg$para)) paste("Para:", paste(cfg$para, collapse = ", ")),
              if (length(cfg$cc))   paste("CC:",   paste(cfg$cc,   collapse = ", ")),
              if (length(cfg$cco))  paste("CCO:",  paste(cfg$cco,  collapse = ", ")))
  paste(partes, collapse = " | ")
}

# Añade una fila a la pestaña de log (la crea con cabecera si no existe)
registrar_envio <- function(ss, modo, dia, casos, cati, cawi, destinatarios, resultado) {
  # destinatarios: texto ya formateado con texto_destinatarios()
  fila <- data.frame(
    fecha_ejecucion = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = TZ),
    modo            = modo,
    periodo         = if (inherits(dia, "Date")) format(dia, "%d/%m/%Y") else dia,
    casos           = casos,
    cati            = cati,
    cawi            = cawi,
    destinatarios   = destinatarios,
    resultado       = resultado,
    stringsAsFactors = FALSE
  )
  tryCatch({
    if (PESTANA_LOG %in% sheet_names(ss)) {
      sheet_append(ss, fila, sheet = PESTANA_LOG)
    } else {
      sheet_write(fila, ss = ss, sheet = PESTANA_LOG)
    }
  }, error = function(e) warning("No se pudo escribir el log en la hoja: ", conditionMessage(e)))
}

# --- SESIÓN LIMESURVEY ---
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

nz <- function(x) if (is.null(x)) "" else as.character(x)

# --- LIMESURVEY ---
get_respuestas <- function(sid) {
  # Parámetros en el orden de la firma PHP de export_responses
  res <- call_limer("export_responses", list(
    iSurveyID         = sid,
    sDocumentType     = "csv",
    sLanguageCode     = "",
    sCompletionStatus = ESTADO,
    sHeadingType      = "code",
    sResponseType     = "short"
  ))
  if (is.list(res)) {                      # p.ej. {status: "No Data, survey table does not exist."}
    message(sid, ": ", nz(res$status))
    return(NULL)
  }
  txt <- rawToChar(jsonlite::base64_dec(res))
  Encoding(txt) <- "UTF-8"
  txt <- sub("^﻿", "", txt)
  cabecera <- sub("\r?\n.*", "", txt)
  sep <- if (lengths(regmatches(cabecera, gregexpr(";", cabecera))) >
             lengths(regmatches(cabecera, gregexpr(",", cabecera)))) ";" else ","
  read.csv(text = txt, sep = sep, colClasses = "character", check.names = FALSE,
           na.strings = character(0))
}

get_participantes <- function(sid) {
  res <- call_limer("list_participants", list(
    iSurveyID   = sid,
    iStart      = 0,
    iLimit      = 1000000,
    bUnused     = FALSE,
    aAttributes = list("attribute_1", "attribute_2", "attribute_5", "attribute_6")
  ))
  if (!is.null(names(res)) && !is.null(res$status)) {
    message(sid, ": ", res$status)
    return(NULL)
  }
  data.frame(
    token       = vapply(res, function(x) nz(x$token), ""),
    attribute_1 = vapply(res, function(x) nz(x$attribute_1), ""),
    attribute_2 = vapply(res, function(x) nz(x$attribute_2), ""),
    attribute_5 = vapply(res, function(x) nz(x$attribute_5), ""),
    attribute_6 = vapply(res, function(x) nz(x$attribute_6), ""),
    stringsAsFactors = FALSE
  )
}

a_madrid <- function(x) {
  if (!FECHAS_UTC) return(x)
  f <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out <- format(f, "%Y-%m-%d %H:%M:%S", tz = TZ)
  out[is.na(f)] <- x[is.na(f)]
  out
}

# Opción marcada en una multirrespuesta: "Y" con respuestas cortas, "Sí" con respuestas completas
marcado <- function(x) x %in% c("Y", "Sí")
col_opcion <- function(cod) paste0(PREGUNTA, "[", cod, "]")

exfe_encuesta <- function(sid) {
  r <- get_respuestas(sid)
  if (is.null(r) || !nrow(r)) return(NULL)
  if (!"token" %in% names(r)) stop(sid, ": no hay columna token (¿encuesta anonimizada?)")
  if (!col_opcion(CODIGO) %in% names(r)) {
    stop(sid, ": no existe la columna ", col_opcion(CODIGO), ". Parecidas: ",
         paste(grep(PREGUNTA, names(r), value = TRUE, fixed = TRUE), collapse = ", "))
  }

  r <- r[marcado(r[[col_opcion(CODIGO)]]), ]
  if (!nrow(r)) return(NULL)

  otros <- intersect(col_opcion(OTROS_CODIGOS), names(r))
  r$otras_opciones <- if (length(otros)) {
    apply(r[, otros, drop = FALSE], 1, function(f) {
      paste(sub(".*\\[(.*)\\]", "\\1", otros[marcado(f)]), collapse = ", ")
    })
  } else ""

  p <- get_participantes(sid)
  d <- merge(r[, c("id", "token", "submitdate", "otras_opciones")], p,
             by = "token", all.x = TRUE)
  sin_part <- sum(is.na(d$attribute_5))
  if (sin_part) warning(sid, ": ", sin_part, " respuestas sin participante asociado")

  test <- grepl(EXCLUIR_TEST, d$token) | grepl(EXCLUIR_TEST, d$attribute_5)
  d <- d[!test, ]
  if (!nrow(d)) return(NULL)                   # p.ej. solo había casos de prueba

  data.frame(
    modo                 = rep(unname(ENCUESTAS[as.character(sid)]), nrow(d)),
    identificador_muestra = d$attribute_6,
    id_respuesta         = d$id,
    tipo_contacto    = d$attribute_1,          # Tipo de solicitud
    canal            = d$attribute_2,          # Tipo canal de entrada
    codigo_solicitud = d$attribute_5,
    fecha_encuesta   = a_madrid(d$submitdate),
    PART_EXFE        = "Sí",
    otras_opciones_PART = d$otras_opciones,
    stringsAsFactors = FALSE
  )
}

# --- CORREO (SMTP de edesga.com vía curl) ---
b64_txt <- function(x) gsub("\n", "", jsonlite::base64_enc(charToRaw(enc2utf8(x))))
b64_raw <- function(x) gsub("\n", "", jsonlite::base64_enc(x))
partir76 <- function(s) {
  if (!nchar(s)) return("")
  ini <- seq(1, nchar(s), 76)
  paste(substring(s, ini, pmin(ini + 75, nchar(s))), collapse = "\r\n")
}
dir_email <- function(x) sub(".*<([^>]+)>.*", "\\1", x)

construir_mensaje <- function(para, cc, asunto, html, adjunto) {
  old <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  fecha <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")
  Sys.setlocale("LC_TIME", old)
  aleat <- paste(sample(c(letters, 0:9), 16, TRUE), collapse = "")
  msgid <- sprintf("<%s.%s@edesga.com>", format(Sys.time(), "%Y%m%d%H%M%S"), aleat)
  frontera <- paste0("=_edesga_", aleat)
  nombre <- basename(adjunto)
  datos  <- readBin(adjunto, "raw", file.size(adjunto))

  paste0(
    "From: ", REMITENTE, "\r\n",
    "To: ", if (length(para)) paste(para, collapse = ", ") else "undisclosed-recipients:;", "\r\n",
    if (length(cc)) paste0("Cc: ", paste(cc, collapse = ", "), "\r\n") else "",
    "Subject: =?UTF-8?B?", b64_txt(asunto), "?=\r\n",
    "Date: ", fecha, "\r\n",
    "Message-ID: ", msgid, "\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: multipart/mixed; boundary=\"", frontera, "\"\r\n",
    "\r\n",
    "--", frontera, "\r\n",
    "Content-Type: text/html; charset=UTF-8\r\n",
    "Content-Transfer-Encoding: base64\r\n",
    "\r\n",
    partir76(b64_txt(html)), "\r\n",
    "--", frontera, "\r\n",
    "Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet; name=\"", nombre, "\"\r\n",
    "Content-Disposition: attachment; filename=\"", nombre, "\"\r\n",
    "Content-Transfer-Encoding: base64\r\n",
    "\r\n",
    partir76(b64_raw(datos)), "\r\n",
    "--", frontera, "--\r\n"
  )
}

# Los CCO van en la lista de entrega (mail_rcpt) pero NO en las cabeceras: nadie los ve
enviar_correo <- function(para, cc, cco, asunto, html, adjunto) {
  tryCatch({
    curl::send_mail(
      mail_from   = dir_email(REMITENTE),
      mail_rcpt   = unique(c(para, cc, cco)),
      message     = construir_mensaje(para, cc, asunto, html, adjunto),
      smtp_server = SMTP_SERVER,
      username    = SMTP_USER,
      password    = SMTP_PASS,
      verbose     = FALSE
    )
    "OK"
  }, error = function(e) paste("error:", conditionMessage(e)))
}

# --- EJECUTAR ---
cfg <- leer_config()
dest_txt <- texto_destinatarios(cfg)
cat(dest_txt, "| Modo:", cfg$modo, "\n")

get_session_key()
lista <- do.call(rbind, lapply(as.integer(names(ENCUESTAS)), exfe_encuesta))
release_session_key()

columnas <- c("modo", "identificador_muestra", "id_respuesta", "tipo_contacto", "canal",
              "codigo_solicitud", "fecha_encuesta", "PART_EXFE", "otras_opciones_PART")
if (is.null(lista)) lista <- setNames(data.frame(matrix(character(0), 0, length(columnas))), columnas)
lista <- lista[order(lista$fecha_encuesta), ]

hoy  <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = TZ))
es_dia_envio <- function(d) as.integer(format(d, "%u")) %in% DIAS_ENVIO
f_es <- function(d) format(d, "%d/%m/%Y")

if (cfg$modo == "diario") {
  # Fuera de los días de envío no se hace nada (evita duplicados si el cron salta en fin de semana)
  if (!es_dia_envio(hoy)) {
    cat("Hoy no es día de envío (DIAS_ENVIO). No se envía nada.\n")
    quit(save = "no")
  }
  # Periodo: desde el último día de envío anterior hasta ayer.
  # Martes-viernes: solo ayer. Lunes: viernes, sábado y domingo.
  hasta <- hoy - 1
  desde <- hasta
  while (!es_dia_envio(desde)) desde <- desde - 1
  dias  <- substr(lista$fecha_encuesta, 1, 10)
  lista <- lista[dias >= format(desde) & dias <= format(hasta), ]
  dia   <- hasta
  texto_fecha <- if (desde == hasta) f_es(hasta) else paste(f_es(desde), "al", f_es(hasta))
  plantilla_asunto <- ASUNTO_DIARIO; plantilla_cuerpo <- CUERPO_DIARIO
} else {
  dia   <- hoy
  texto_fecha <- f_es(hoy)
  plantilla_asunto <- ASUNTO_TOTAL;  plantilla_cuerpo <- CUERPO_TOTAL
}
# El Excel va a una carpeta temporal: solo existe para adjuntarlo y se borra al cerrar R
xlsx <- file.path(tempdir(), sprintf("EXFE_%s_%s.xlsx", cfg$modo, format(dia, "%Y%m%d")))

valores <- list(fecha = texto_fecha,
                total = nrow(lista),
                cati  = sum(lista$modo == "CATI"),
                cawi  = sum(lista$modo == "CAWI"))
asunto <- rellenar(plantilla_asunto, valores)
html   <- rellenar(plantilla_cuerpo, valores)

writexl::write_xlsx(lista, xlsx)
if (!APLICAR) file.copy(xlsx, basename(xlsx), overwrite = TRUE)   # en pruebas, copia local para revisarlo
cat("Asunto:", asunto, "\nCasos:", valores$total, "(CATI:", valores$cati, "| CAWI:", valores$cawi, ")",
    "| Excel:", xlsx, "|", if (APLICAR) "APLICAR" else "DRY-RUN", "\n")
print(head(lista, 10))

if (!nrow(lista) && cfg$modo == "diario" && !ENVIAR_SI_VACIO) {
  cat("No hubo casos del", texto_fecha, ". No se envía nada.\n")
  if (APLICAR) registrar_envio(cfg$ss, cfg$modo, texto_fecha, 0, 0, 0, dest_txt, "sin casos, no enviado")
  quit(save = "no")
}

if (APLICAR) {
  if (SMTP_USER == "" || SMTP_PASS == "") stop("Faltan SMTP_USER / SMTP_PASS")

  res <- enviar_correo(cfg$para, cfg$cc, cfg$cco, asunto, html, xlsx)
  cat("Envío:", res, "\n")

  registrar_envio(cfg$ss, cfg$modo, texto_fecha, valores$total, valores$cati, valores$cawi,
                  dest_txt, res)

  if (res != "OK") stop("No se pudo enviar el correo: ", res)
}