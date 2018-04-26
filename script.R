#### settings ####

## set wait period
wait_time_in_seconds = 60

## set maximum error count
max_error_count = 10



#### mobile-friendliness tests in bulk ####

## import api key
source('api_key.R')

## set service URL
service_url = 'https://searchconsole.googleapis.com/v1/urlTestingTools/mobileFriendlyTest:run'

## import libraries
library(glue)
library(httr)
library(lubridate)
library(tidyverse)

## import spreadsheet
input_df = read.csv("urls.csv", stringsAsFactors=FALSE)

## separate into those with and without URLs
urls_df = subset(input_df, !grepl('no website', url))
no_urls_df = subset(input_df, grepl('no website', url))


## prepend www to urls if not in place
urls_df$url = ifelse(!grepl('www\\.', urls_df$url),
                     paste0('www.', urls_df$url),
                     urls_df$url)

## prepend http:// to urls if not in place
urls_df$url = ifelse(!grepl('http://|https://', urls_df$url),
                     paste0('http://', urls_df$url),
                     urls_df$url)

## place N/A when there are no urls
no_urls_df$testStatus = no_urls_df$mobileFriendliness = 'N/A'

## initialize error counter
error_count = 0

## as long as there is an unperformed mobile-friendliness test
while (NA %in% urls_df$testStatus & error_count < max_error_count) {
  
  ## calculate the total number of urls to test
  n_cases = sum(is.na(urls_df$testStatus))
  
  ## get url to test
  url = urls_df$url[is.na(urls_df$testStatus)][1]
  
  ## get index of selected url's placement in the df
  n = which(urls_df$url==url)
  
  ## print to console to show progress
  print(glue('Performing mobile-friendly test for: {url} ({n_cases} remaining)'))    
  
  ## make post request to service URL
  r = POST(service_url, query = list(url=url, key=api_key))  
  
  ## save response content
  response_content = content(r)
  
  ## if test was completed 
  if (!is.null(response_content$testStatus$status)) {
    urls_df[n, 'testStatus'] = response_content$testStatus$status
    urls_df[n, 'mobileFriendliness'] = ifelse(response_content$testStatus$status=='COMPLETE', 
                                              response_content$mobileFriendliness, NA)
  } 
  
  ## if there was an issue
  else {
    Sys.sleep(wait_time_in_seconds) # wait before resuming 
  }
  
  ## if this was the last test
  if (n_cases==1) {
    
    ## print completion message
    print(glue("Tests complete."))
    break
  }
  
  ## if there are more tests to perform
  else {
    Sys.sleep(wait_time_in_seconds) # wait before resuming
  }
}

## create output df
output_df = rbind.data.frame(urls_df, no_urls_df)

## export the test result in csv
write.csv(output_df, "urls.csv", row.names=FALSE)

