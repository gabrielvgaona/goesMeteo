source("R/functions.R")

goes_auth()

data <- request_goes(
  DcpAddr = "393432EE", 
  StartDt = format(Sys.Date() - days(30), "%m/%d/%Y"),
  EndDt = format(Sys.Date(), "%m/%d/%Y"), 
  HoursDt = 0)

data |> 
distinct(ts.ini, .keep_all = TRUE) |> 
  view()
   #   as_tibble() |> 
#   arrange(ts.ini)

write_csv(data, file.path("data", format(Sys.time(), "%Y%m%d%H%M_m5148_goes.csv")))
