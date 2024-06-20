library(httr2)
library(tidyverse)


#' @import httr2 map map_dfr
pbin2int <- function(x){
    n <- utf8ToInt(x) |> 
        signif(8) |> 
        intToBits() |> 
        rev() |>
        as.numeric()
    n[(length(n)-6 + 1):length(n)]
}

bin2dec <- function(x){
    p <- (length(x):1)-1
    b <- 2^p
    sum(x * b)
}

decodeGoesTriad <- function(x){
    if(x == "///") return(-9999)
    str_split(x, "") |>
        unlist() |> 
        lapply(pbin2int) |> 
        do.call("c", args = _) |> 
        bin2dec()
}

scale_10 <- function(x, scl.pow, na.val = -9999){
    scl.pow[x == na.val] <- 0
    x * 1/10^scl.pow
}

parseGoesString <- function(x, 
                       vars = f10$vars, 
                       dec.pos = f10$dec) {
  nc <- nchar(x)
  if(nc != length(vars) * 3) {
    x <- str_sub(x, 1, length(vars) * 3)
    x <- str_sub(x, 1, (nc %/% 3) * 3)
    x <- str_pad(x, length(vars) * 3, side = "right", "/")
    }
  ini <- 3 * (1:length(vars)) - 2
  fin <- 3 * (1:length(vars))
  x |> 
      stringr::str_sub(ini, fin) |> 
      rlang::set_names(nm = vars) |> 
      purrr::map_dbl(decodeGoesTriad) |> 
      scale_10(dec.pos)
}
    
goes_auth <- function(){
    tryCatch(
    Sys.setenv(GOES_USER = rstudioapi::showPrompt("User", "Please enter your DSC GOES username: ")),
    error = function(e) stop("User must not be empty!")
    )
    tryCatch(
    Sys.setenv(GOES_PASS = askpass::askpass()),
    error = function(e) stop("Password must not be empty!")
    )
    message("GOES credentials correctly configured!")
}

parse_msg <- function(x){
    time.ini <- (readr::parse_number(x$TblDcpDataDtMsgCar)/1000) |> 
        as.POSIXct(tz = "UTC")
    time.end <- (readr::parse_number(x$TblDcpDataDtMsgEnd)/1000) |> 
        as.POSIXct(tz = "UTC")
    #print(x$TblDcpDataData)
    x$TblDcpDataData |>
      stringr::str_trim() |> 
      check() |> 
      stringr::str_split(" ") |> 
      unlist() |> 
      stringr::str_trim() |> 
      purrr::map_dfr(parseGoesString) |> 
      dplyr::mutate(ts.ini = lubridate::floor_date(c(time.ini - hours(1), time.ini)), 
               ts.end = lubridate::floor_date(c(time.end - hours(1), time.end)), 
               .before = 1) 
}


request_goes <- function(...,
                         baseurl = "https://dcs1.noaa.gov",
                         path = "/account/fieldtest/"){
    
    url <- httr2::url_parse(baseurl)
    url$path <- path
    dots <- list(...)
    
    if(Sys.getenv("GOES_USER") == "" | Sys.getenv("GOES_PASS") == "") 
      rlang::abort("GOES credentials required!\n  Please use goes_auth() to setup your credentials")
    
    dots$UserName <- Sys.getenv("GOES_USER")
    dots$Password <- Sys.getenv("GOES_PASS")
    
    url <- httr2::url_build(url)
    req <- httr2::request(url) |> 
        httr2::req_method("POST") |> 
        httr2::req_headers("Accept" = "application/json") |> 
        httr2::req_body_json(dots)
    resp <- httr2::req_perform(req)
    
    if(httr2::resp_is_error(resp)) return(httr2::resp_check_status(resp))
    
    data <- httr2::resp_body_json(resp)
    
    if(exists("success", data) & !data$success) rlang::abort(data$error)
    
    purrr::map_dfr(data$msgs, parse_msg)
}

check <- function(x, pattern = "^@"){
  if(!stringr::str_detect(x, pattern = pattern)){
    x <- stringr::str_remove(x, ".+?(?=@)")
  }
  
  t <- stringr::str_split(x, " ")
  if(length(unlist(t)) < 2)  x <- paste(x, x)
  x
}

f10 <- list(
    vars = c("TAavg", "TAmax", "TAmin", "RHavg", "RHmax", "RHmin", 
             "PAavg", "PAmax", "PAmin", "Sum_PR", "RSavg", "RSmax", 
             "RSmin", "RS_sum", "TG1avg", "TG1max", "TG1min", "TG2avg", 
             "TG2max", "TG2min", "TG3avg", "TG3max", "TG3min", 
             "TG4avg", "TG4max", "TG4min", "TG5avg", "TG5max", 
             "TG5min", "TG6avg", "TG6max", "TG6min", "TG7avg", 
             "TG7max", "TG7min", "WRun", "DIRavg", "DIRmax", "DIRmin", 
             "SPDavg", "SPDmax", "SPDmin", "GUSTdir", "GUSTh", "GUSTm", "Vbat"),
    dec = c(1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 
            1, 1, 1, 0, 0, 0, 2)
)
