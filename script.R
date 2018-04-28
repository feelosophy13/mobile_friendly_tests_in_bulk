#### settings ####

## set wait period
wait_time_in_seconds = 10

## set maximum error count per url
max_per_url_test_attempts = 3



#### perform mobile-friendliness tests in bulk ####

## import api keys
source('api_keys.R')

## set service URL
service_url = 'https://searchconsole.googleapis.com/v1/urlTestingTools/mobileFriendlyTest:run'

## import libraries
library(glue)
library(httr)


## import spreadsheet
urls_df = read.csv("urls.csv", stringsAsFactors=FALSE)

## remove completely blank rows
urls_df = urls_df[rowSums(is.na(urls_df)) != ncol(urls_df),]

## lowercase urls
urls_df$url <- tolower(urls_df$url)

## remove "no website" rows
urls_df = subset(urls_df, !grepl('no website', urls_df$url))

## prepend www to urls if not in place
urls_df$url = ifelse(!grepl('no website', urls_df$url) & !grepl('www\\.', urls_df$url),
                     paste0('www.', urls_df$url),
                     urls_df$url)

## prepend http:// to urls if not in place
urls_df$url = ifelse(!grepl('no website', urls_df$url) & !grepl('http://|https://', urls_df$url),
                     paste0('http://', urls_df$url),
                     urls_df$url)

## calculate number of urls to test
n_urls = nrow(urls_df)

## initialize total query count
total_query_count = 0

## for each url
for (i in 1:n_urls) {

  ## get url to test
  url = urls_df$url[i]

  ## print to console to show progress
  print(glue('Performing mobile-friendliness test for {url} ({i} out of {n_urls})'))   

  ## initialize number of url test attempts
  n_url_test_attempts = 0
  
  ## before the test is completed and before we reach the per-url attempt limit
  while (n_url_test_attempts < max_per_url_test_attempts) {
    
    ## increment total query count
    total_query_count = total_query_count + 1
    
    ## increment test attempt count
    n_url_test_attempts = n_url_test_attempts + 1
    
    ## get api key to use
    api_key_index = (total_query_count - 1) %% length(api_keys) + 1
    api_key = api_keys[api_key_index]
    
    ## print to console to show progress
    print(glue('Attempt #{n_url_test_attempts}, using API key {api_key}'))
    
    ## make post request to service URL
    r = POST(service_url, query = list(url=url, key=api_key))  
      
    ## save response content
    response_content = content(r)        
      
    ## if test was completed 
    if (!is.null(response_content$testStatus$status)) {
        urls_df[i, 'testStatus'] = response_content$testStatus$status
        urls_df[i, 'mobileFriendliness'] = ifelse(response_content$testStatus$status=='COMPLETE', 
                                                  response_content$mobileFriendliness, NA)
        
        ## break out of the while loop to stop testing the current url
        break
    }
    
    ## if there was a known error
    else if (!is.null(response_content$error)) {

      ## print the error message
      print(response_content$error$message)
      
      ## if program should make more test attempts      
      if (n_url_test_attempts < max_per_url_test_attempts) {

        ## wait before trying again
        Sys.sleep(wait_time_in_seconds) 
      }
      
      ## if this is the final attempt
      else {
        
        ## log the error in output df
        urls_df[i, 'testStatus'] <- response_content$error$message
        urls_df[i, 'mobileFriendliness'] <- response_content$error$message
        
        ## break out of the while loop to stop testing the current url
        break
      }
    } 
    
    ## if there was an unknown error
    else {
      
      ## print the error message
      print(glue('Unknown error encountered while testing {url}'))
      
      ## if program should make more test attempts      
      if (n_url_test_attempts < max_per_url_test_attempts) {
        
        ## wait before trying again
        Sys.sleep(wait_time_in_seconds) 
      }
      
      ## if this is the final attempt
      else {
        
        ## log the error in output df
        urls_df[i, 'testStatus'] <- 'UNKNOWN_ERROR_ENCOUNTERED'
        urls_df[i, 'mobileFriendliness'] <- 'UNKNOWN_ERROR_ENCOUNTERED'
        
        ## break out of the while loop to stop testing the current url
        break
      }
      
    }

  } 

  ## print to console to delimit progress
  print(glue('---------------------------'))
  
  ## if this was the last url to test
  if (i==n_urls) {
    
    ## print completion message
    print(glue("Tests complete."))
  }
  
  ## if there are more tests to perform
  else {
    
    ## wait before moving onto the next url
    Sys.sleep(wait_time_in_seconds) 
  }
  
}

## export the test result in csv
write.csv(urls_df, "urls.csv", row.names=FALSE)
