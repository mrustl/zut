#' ZUT25 URLS for Each Track
#'
#' @returns list with track short name and url
#' @export
#'
zut25_urls <- function()  {
  list(zut25_ultratrail = "https://www.datasport.de/anmeldeservice/147/2845/9954",
       zut25_ehrwald = "https://www.datasport.de/anmeldeservice/147/2845/9955",
       zut25_leutasch = "https://www.datasport.de/anmeldeservice/147/2845/9956",
       zut25_mittenwald = "https://www.datasport.de/anmeldeservice/147/2845/9957",
       zut25_garmisch = "https://www.datasport.de/anmeldeservice/147/2845/9958",
       zut25_grainau = "https://www.datasport.de/anmeldeservice/147/2845/9960")
}


#' ZUT25 check for available starting places for each track
#'
#' @param url one element of \code{\link{zut25_urls}}
#' @param recipient_email  recipient email (default: Sys.getenv("NOTIFICATION_RECIPIENT_EMAIL"))
#' @param send_email should notifiation emails be sent (default: FALSE)
#' @returns either 0 (if no starting place is available) or number of available 
#' places as text. in additon an email will be sent to recipient email in case 
#' a starting place is available
#' @export
#' @examples
#' sapply(zut25_urls(), check_starting_places)
#' 
#' @importFrom httr content GET status_code
#' @importFrom rvest html_node html_text  
check_starting_places <- function(url, 
                                  recipient_email = Sys.getenv("NOTIFICATION_RECIPIENT_EMAIL"),
                                  send_email = FALSE) {

# Webseite abrufen
website <- httr::GET(url[[1]])


# Ueberprüfen, ob die Anfrage erfolgreich war
if (httr::status_code(website) == 200) {
  # Inhalt der Webseite als HTML parsen
  parsed_html <- rvest::read_html(httr::content(website, as = "text", encoding = "UTF-8"))
  
  # Alle "ym-gbox panibox"-Elemente finden
  panibox_nodes <- parsed_html %>%
    rvest::html_nodes(".ym-gbox.panibox")
  
  # Filtere nur die Knoten, die ein <h6>-Element mit "Startplatzboerse" enthalten
  filtered_nodes <- panibox_nodes[sapply(panibox_nodes, function(node) {
    h6_text <- node %>% 
      rvest::html_node("h6") %>% 
      rvest::html_text(trim = TRUE)
    return(!is.na(h6_text) && h6_text == "Startplatzb\u00F6rse")
  })]
  
  # Extrahiere den relevanten Text aus den gefilterten Boxen
  extracted_text <- filtered_nodes %>%
    rvest::html_node("div") %>% 
    rvest::html_node("span") %>% 
    rvest::html_text(trim = TRUE)
  
  # Ausgabe des extrahierten Textes
  if (tolower(extracted_text) == "kein") {
    message(sprintf("Curently there is NO starting place available (%s)", Sys.time()))
    0
    
  } else {
    txt <- sprintf("Curently there are %s starting place(s) available (%s). Visit '%s' as soon as possible", 
                   extracted_text, 
                   Sys.time(),
                   url)
    message(sprintf("Aktuell sind %s Startplatz(e) verfügbar (%s)", 
                    extracted_text, 
                    Sys.time()))
    
    send_email_notification(to = recipient_email, 
                            subject = sprintf("Startplatz verfügbar fuer '%s'!",
                                              names(url)), 
                            body = txt)
    
    return(extracted_text)
    
  }
  
}
}

