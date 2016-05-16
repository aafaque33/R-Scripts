#-------------------------------------------------------------------
# Author/Developer: Aafaque Aafaque
# Email: aafaqueabdullah@yahoo.com
#-------------------------------------------------------------------

#############################################################################
# Code to download weather history      
# Download Data suing API and store in files (each month file in json format)
#############################################################################

start = "01/01/09"
end = ""
api_key = "Your API Key"
state = "SD"
city = "Vermillion"

downloadFile(start)

downloadFile <- function(start)
{
  for (i in 1:50 ) # this loop is there because for simple API key we were allowed to do only 50 transactions at a time
  {
    getwd()
    end <- as.Date(start, "%d/%m/%y") + 30
    datestart <- as.Date(start, "%d/%m/%y")
    datestart <- gsub("-","",datestart)
    dateend <- as.Date(end, "%d/%m/%y")
    dateend <- gsub("-","",dateend)
    url <- paste("http://api.wunderground.com/api/",api_key,"/history_",datestart,dateend,"/q/",state,"/",city,".json ")
    url <- gsub(" ","",url)
    file <- download.file(url,paste(datestart,dateend),method = "auto", quiet = FALSE, mode = "w")
    start = as.Date(start,"%d/%m/%y") + 31
  }
  return (url)
}