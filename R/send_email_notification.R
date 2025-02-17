
# SMTP Einstellungen für den E-Mail-Versand
#' SMTP configuration for email notification 
#'
#' @returns list with smtp configuration
#' @export
#'
notification_smtp_settings <- function() {

  list(
    host.name = Sys.getenv("NOTIFICATION_SERVER"),  # Ersetze mit deinem SMTP-Server
    port = as.numeric(Sys.getenv("NOTIFICATION_PORT")),                      # Port (meist 465 oder 587)
    user.name = Sys.getenv("NOTIFICATION_EMAIL"),   # Deine E-Mail-Adresse
    passwd = Sys.getenv("NOTIFICATION_PW"),        # Dein Passwort (oder App-Passwort)
    ssl = as.logical(Sys.getenv("NOTIFICATION_USE_SSL"))
)
}

#' Send email notification
#'
#' @param to to
#' @param subject subject 
#' @param body body
#' @param smtp_settings smtp_settings as retrieved by \code{\link{smtp_settings}} 
#'
#' @returns sends email
#' @export
#'
#' @importFrom sendmailR mime_part sendmail
send_email_notification <- function(to, subject, body, smtp_settings = notification_smtp_settings()) {

  from <- smtp_settings$user.name
  msg <- sendmailR::mime_part(body)
  email <- sendmailR::mime_part("")
  email$headers$To <- to
  email$headers$From <- from
  email$headers$Subject <- subject
  email$body <- list(msg)
  
  sendmailR::sendmail(from = from, 
                      to = to,
                      subject = subject,
                      msg = email,
                      engine = "curl", 
                      engineopts = list(username = smtp_settings$user.name, password = smtp_settings$passwd),
                      control = list(smtpServer=sprintf("smtp://%s:%d", smtp_settings$host.name, smtp_settings$port), verbose = TRUE))
}
