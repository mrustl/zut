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
#' @param send_pushover_notification should pushover notifiation be sent (default: FALSE)
#' @returns either 0 (if no starting place is available) or number of available 
#' places as text. in additon an email will be sent to recipient email in case 
#' a starting place is available
#' @export
#' @examples
#' sapply(zut25_urls(), check_starting_places)
#' 
#' @importFrom httr content GET status_code
#' @importFrom rvest html_node html_text  
#' @importFrom pushoverr pushover_emergency
check_starting_places <- function(url, 
                                  send_pushover_notification = FALSE) {

# Webseite abrufen
website <- httr::GET(url[[1]])

# Ueberprüfen, ob die Anfrage erfolgreich war
res <- if (httr::status_code(website) == 200) {

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
    message(sprintf("%s: currently there is NO starting place available (%s)", 
                    names(url), 
                    Sys.time()))
    0
    
  } else {
    txt <- sprintf("%s: currently there are %s starting place(s) available (%s). Visit %s as soon as possible", 
                   names(url),
                   extracted_text, 
                   Sys.time(),
                   url)
    
   if(send_pushover_notification) {
    pushoverr::pushover_emergency(message = txt, 
                                  url = as.character(url),
                                  url_title = sprintf("Starting place main website for '%s'",
                                                      names(url)))
   }  
    
    extracted_text
    
  }
  
}
  
  return(res)  
}

