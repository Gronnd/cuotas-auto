library(mailR)
library(googlesheets4)
library(officer)

send_sheet_content <- function(sheet_url, emails) {
  
  # Crear un documento de Word
  doc <- read_docx()
  
  # Leer datos de las hojas "cuotas" y "cuotas_encuestadores" de Google Sheets
  cuotas <- read_sheet(sheet_url, sheet = "cuotas")
  cuotas_encuestadores <- read_sheet(sheet_url, sheet = "cuotas_encuestadores")
  
  # Agregar los datos de "cuotas" y "cuotas_encuestadores" al documento de Word
  doc <- doc %>%
    body_add_par(paste("Datos de la hoja 'cuotas' del libro:", sheet_url), style = "heading 1") %>%
    body_add_table(cuotas, style = "table 1") %>%
    body_add_par(paste("Datos de la hoja 'cuotas_encuestadores' del libro:", sheet_url), style = "heading 1") %>%
    body_add_table(cuotas_encuestadores, style = "table 1")
  
  # Guardar el documento de Word
  temp_file <- tempfile(fileext = ".docx")
  print(doc, target = temp_file)
  
  # Configurar y enviar correo electrónico
  send.mail(
    from = "tu-email@example.com",
    to = emails,
    subject = paste("Contenido de las Hojas de Cálculo del libro:", sheet_url),
    body = "Encuentra adjunto el contenido de las hojas de cálculo.",
    smtp = list(host.name = "smtp.gmail.com", port = 587,
                user.name = "tu-email@example.com",            
                passwd = "tu-contraseña", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE,
    attach.files = temp_file,
    file.names = paste(basename(temp_file), ".docx", sep = "")
  )
}

# Lista de URLs de hojas de cálculo
sheet_urls <- c("url-hoja-1", "url-hoja-2", ...)

# Lista de correos electrónicos
emails <- c("correo1@example.com", "correo2@example.com", ...)

# Enviar contenido de hojas de cálculo
mapply(send_sheet_content, sheet_urls, list(emails))

