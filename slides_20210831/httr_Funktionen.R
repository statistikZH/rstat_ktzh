require(httr) # for getting headers and parsing server responses

###Funktionen########################################################################################
#domain in URL (nicht verwendet)
get_written_domain <- function(x){
  xspl <- strsplit(x, split=".", fixed=TRUE)
  xdom <- xspl[[1]][(lengths(xspl)-2):(lengths(xspl))]
  return(xdom)
}

#Funktion um domains abzurufen (benutzt um domains abzurufen, aus http_domain_changed gebaut)
http_domains <- 
  function(response){
    tryCatch({ #damit kein unterbruch wenn error
      # extract location headers
      location <- 
        unlist(
          lapply(
            X   = response$all_headers, 
            FUN = 
              function(x){
                x$headers$location
              }
          )
        )
      
      # new domain
      new_domain <<- urltools::domain(location)
      return(new_domain)
    }, error = function(e) {
      return(0)
    })
  }

r <- httr::GET("ajb.zh.ch")
http_domains(r)

#Funktion um zu prüfen, ob sich domain beim Aufruf ändert (nicht verwendet)
http_domain_changed <- 
  function(response){
    # get domain of origignal HTTP request
    orig_domain <- urltools::domain(response$request$url)
    
    # extract location headers
    location <- 
      unlist(
        lapply(
          X   = response$all_headers, 
          FUN = 
            function(x){
              x$headers$location
            }
        )
      )
    
    # new domains
    new_domains <<- urltools::domain(location)
    
    # check domains in location against original domain
    any( !is.na(new_domains) & new_domains != orig_domain )
  }

r <- httr::GET("www.zh.ch")

http_domain_changed(r)

#Funktion hostnamen auslesen (nicht verwendet)
get_hostname <- function(href) {
  tryCatch({
    parsed_url <- parse_url(href)
    if (!parsed_url$hostname %>% is.null()) {
      hostname <- parsed_url$hostname %>% 
        gsub('^www.', '', ., perl = T)
      return(hostname)  
    } else {
      return('unresolved')
    }
    
  }, error = function(e) {
    return('unresolved')
  })
}

get_hostname(r)

#Funktion URL-Response (verwendet um status codes auszulesen)
check_url_response <- function(href) {
  cat('Checking', href, '...\n')
  tryCatch({
    check_head <- HEAD(href)
    return(check_head$status_code %>% as.integer())
  }, error = function(e) {
    return(0)
  })
}

check_url_response("zh.ch")

#Funktion Ziel-URL abrufen (verwendet um Ziel-URL abzurufen)
catch_ziel_url <- function(subdomain) {
  cat('Checking', subdomain, '...\n')
  
  time_limit <- 10 #anzahl sekunden bis zeile auslassen
  
  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch({
    check_url <- httr::GET(as.character(subdomain))
    return(check_url$url %>% as.character())
  }, error = function(e) {
    return(0)
  })
}

catch_ziel_url("ajb.zh.ch")
