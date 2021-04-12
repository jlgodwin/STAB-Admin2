##########
#  Using googlesheets
#
##########

#### Libraries ####
if(!('googlesheets4' %in% .packages(all.available = TRUE))){
  install.packages('googlesheets4', dependencies = T)
}

# if(!('googledrive' %in% .packages(all.available =  TRUE))){
#   install.packages('googledrive', dependencies = T)
# }
  library(googlesheets4)
# library(googlesheets)
# library(googledrive)

# api.key <- "AIzaSyBERuU0kk_sfA7m-kJcChA_uONnPJUv5Qs"
# 
# google.sheets.url <- "https://www.googleapis.com/auth/spreadsheets"

#### Authenticate your account ####
## Authenticate your acocount
## NOTE: you must have a web browser open.
#token <- gs_auth(cache = FALSE)
# gd_token()
 
gargle::token_fetch(scopes = "https://www.googleapis.com/auth/spreadsheets")
# sheet_key<- gs_url("https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=1656161984",
#                    lookup = TRUE, visibility = "public")

sheet_key <-  as_sheets_id("https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=1656161984")
