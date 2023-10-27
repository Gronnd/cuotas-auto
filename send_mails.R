library(googlesheets4)
library(officer)
library(blastula)
library(googledrive)




# Configurar las credenciales SMTP
smtp_creds <- creds_envvar(
  user = "jorge.acuna@edesga.com",
  host = "mail.edesga.com",
  port = 465,
  use_ssl = TRUE,
  pass_envvar = "SMTP_PASSWORD"
)




# autentificarme con gargle con el json de la cuenta de servicio
drive_auth(path = "C:/Users/Jorge/Documents/GitHub/cuotas-auto/limesurvey-379408-91651184e9db.json", gargle::gargle_oauth_email())
gs4_auth(path = "C:/Users/Jorge/Documents/GitHub/cuotas-auto/limesurvey-379408-91651184e9db.json", gargle::gargle_oauth_email())


send_sheet_content <- function(sheet_url, emails) {
  # Crear un documento de Word
  doc <- read_docx()

  # Intentar leer y añadir datos de la hoja "cuotas"
  tryCatch(
    {
      cuotas <- read_sheet(sheet_url, sheet = "cuotas")
      doc <- doc %>%
        body_add_par(paste("Datos de la hoja 'cuotas' del libro:", sheet_url), style = "heading 1") %>%
        body_add_table(cuotas, style = NULL)
    },
    error = function(e) {
      doc <- doc %>%
        body_add_par(paste("No se encontró la hoja 'cuotas' en el libro:", sheet_url), style = "heading 2")
    }
  )

  # Intentar leer y añadir datos de la hoja "cuotas_encuestadores"
  tryCatch(
    {
      cuotas_encuestadores <- read_sheet(sheet_url, sheet = "cuotas_encuestadores")
      doc <- doc %>%
        body_add_par(paste("Datos de la hoja 'cuotas_encuestadores' del libro:", sheet_url), style = "heading 1") %>%
        body_add_table(cuotas_encuestadores, style = NULL)
    },
    error = function(e) {
      doc <- doc %>%
        body_add_par(paste("No se encontró la hoja 'cuotas_encuestadores' en el libro:", sheet_url), style = "heading 2")
    }
  )

  # Guardar el documento de Word
  temp_file <- tempfile(fileext = ".docx")
  print(doc, target = temp_file)

  # Componer el correo electrónico
  email <- compose_email(
    body = "Encuentra adjunto el contenido de las hojas de cálculo."
  ) %>%
    add_attachment(file = temp_file, filename = "Datos.docx")

  # Enviar el correo electrónico
  email %>%
    smtp_send(
      from = "jorge.acuna@edesga.com",
      to = emails,
      subject = paste("Contenido de las Hojas de Cálculo del libro:", sheet_url),
      credentials = smtp_creds
    )
}

# Lista de URLs de hojas de cálculo
sheet_urls <- c(
  "https://docs.google.com/spreadsheets/d/14d6cJehg9FmoQKqlBlMFgRTQZ9EcA2hmOfhb84FsOR0",
  "https://docs.google.com/spreadsheets/d/1Fww1AJwt3y-13zjgrmH38gvWMLuMEuXkRw1TacbNiSU"
)

# Lista de correos electrónicos
emails <- c("jrg.ledo@gmail.com", "jorge.acuna@edesga.com")

# Enviar contenido de hojas de cálculo
mapply(send_sheet_content, sheet_urls, list(emails))
