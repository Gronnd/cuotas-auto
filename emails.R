library(googlesheets4)
library(gmailr)
library(officer)
library(jsonlite)

# Autenticación (es posible que necesites configurar la autenticación OAuth 2.0)
 gs4_auth(path = "C:/Users/Jorge/Documents/GitHub/cuotas-auto/limesurvey-379408-91651184e9db.json")
 gm_auth(path = "C:/Users/Jorge/Documents/GitHub/cuotas-auto/limesurvey-379408-024781e8be3d.json")

gm_auth(email = "edesgaenquisas@gmail.com")

gm_deauth( )
gm_has_token()
gm_profile()


send_sheet_content <- function(sheet_url, emails) {
  # Leer datos de las hojas "cuotas" y "cuotas_encuestadores" de Google Sheets
  cuotas <- read_sheet(sheet_url, sheet = "cuotas")
  cuotas_encuestadores <- read_sheet(sheet_url, sheet = "cuotas_encuestadores")
  
  # Crear un documento de Word
  doc <- read_docx()
  doc <- doc %>%
    body_add_par("Datos de la hoja 'cuotas':", style = "heading 1") %>%
    body_add_table(cuotas, style = NULL) %>%
    body_add_par("Datos de la hoja 'cuotas_encuestadores':", style = "heading 1") %>%
    body_add_table(cuotas_encuestadores, style = NULL)
  
  # Guardar el documento de Word
  temp_file <- tempfile(fileext = ".docx")
  print(doc, target = temp_file)
  
  # Configurar y enviar correo electrónico
  email <- gm_mime() %>%
    gm_to(emails) %>%
    gm_subject("Contenido de las Hojas de Cálculo") %>%
    gm_attach_file(temp_file, name = "Datos.docx")
  
  gm_send_message(email)
}

# Lista de URLs de hojas de cálculo
sheet_urls <- c("https://docs.google.com/spreadsheets/d/1zXAbl5EaPY65ATBBfPT78W6IfH3H6LgnJ8tGrk4ZzLU/", "https://docs.google.com/spreadsheets/d/1zXAbl5EaPY65ATBBfPT78W6IfH3H6LgnJ8tGrk4ZzLU/")

# Lista de correos electrónicos
emails <- c("jrg.ledo@gmail.com")

# Enviar contenido de hojas de cálculo
mapply(send_sheet_content, sheet_urls, list(emails))

send_sheet_content <- function(sheet_url, emails) {
  # ... (tu código anterior aquí) ...
  
  tryCatch({
    gm_send_message(emails)
  }, error = function(e) {
    print(e)
  })
}

# ... (resto de tu código
