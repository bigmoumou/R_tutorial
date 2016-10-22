library(httr)
library(rvest)
library(xml2)
library(gsubfn)
library(magrittr)
library(xmlview)
library(jsonlite)
#===================================================================================
urllist <- list()
num = 0
count = 0
buttom = TRUE
while (buttom){
  tryCatch({
    for(eachpage in count:640){ # 640*20=12800
    
      url <- paste0("https://rent.591.com.tw/index.php?module=search&action=rslist&is_new_list=1&type=1&searchtype=1&region=1&orderType=desc&shType=list&firstRow=",num)
      res <- GET(url)
      res_text <- content(res, as = "text", encoding = "UTF-8")
      res_text <- fromJSON(res_text)
      main <- res_text$main
      tmpurl <- unique(strapplyc(main, 'rent-detail-+[0-9]+.html')[[1]])
      tmpurl <- paste0('https://rent.591.com.tw/',tmpurl)
      urllist <- append(urllist, tmpurl)
      num=num+20
      count=count+1
      print(count)}
    
    buttom = FALSE
  
  }, error = function(err) {
      num <- count
     }
  )
  print(paste("Another RUN"))
  Sys.sleep(30)
}
urllist <- unique(unlist(urllist))

#===================================================================================
fulldata <- list()
errorurl <- list()
n = 1
for(eachurl in urllist[n:2000]){  # 12820 = length(urllist)
  
  tryCatch({res <- GET(eachurl) %>% content("parse")},
           error = function(err) {print(paste("errorurl :", eachurl, sep=" "))
                                  errorurl <- append(errorurl,eachurl)
             }
                                 
  )
  
  # extract name :
  xpath = '//*[@class="houseInfoTitle"]'
  tryCatch({tmp_name <- res %>% html_node(xpath=xpath) %>% html_text()},
           error = function(err) {tmp_name <- " "}
  )
  
  # extract price :
  xpath = '//*[@class="price clearfix"]'
  tryCatch({tmp_price <- res %>% html_node(xpath=xpath) %>% html_text()
  tmp_price <- gsub("\\s","",tmp_price)},
  error = function(err) {tmp_price <- " "}
  )
  
  # extract address :
  xpath = '//*[@class="addr"]'
  tryCatch({tmp_address <- res %>% html_node(xpath=xpath) %>% html_text()},
           error = function(err) {tmp_address <- " "}
  )
  
  if(is.na(tmp_name)|is.na(tmp_price)|is.na(tmp_address)){
    break
  }
  
  # extract basic info 1 :
  xpath = '//*[@class="attr"]'
  tryCatch({tmp_info <- res %>% html_node(xpath=xpath) %>% html_children()},
           error = function(err) {
             tmp_feet <- " "
             tmp_floor <- " "
             tmp_type <- " "
             tmp_status <- " "}
  )
  
  tryCatch({tmp_feet <- tmp_info[1] %>% html_text()},
           error = function(err) {tmp_feet <- " "}
  )
  tryCatch({tmp_floor <- tmp_info[2] %>% html_text()},
           error = function(err) {tmp_floor <- " "}
  )
  tryCatch({tmp_type <- tmp_info[3] %>% html_text()},
           error = function(err) {tmp_type <- " "}
  )
  tryCatch({tmp_status <- tmp_info[4] %>% html_text()},
           error = function(err) {tmp_status <- " "}
  )
  
  # extract basic info 2 :
  xpath = '//*[@id="main"]/div[2]/div[2]/div[1]/ul'
  tryCatch({tmp_info <- res %>% html_node(xpath=xpath) %>% html_children()},
           error = function(err) {
             tmp_info1 <- " "
             tmp_info2 <- " "
             tmp_info3 <- " "
             tmp_info4 <- " "}
  )
  
  tryCatch({tmp_info1 <- tmp_info[1] %>% html_text()},
           error = function(err) {tmp_info1 <- " "}
  )
  tryCatch({tmp_info2 <- tmp_info[2] %>% html_text()},
           error = function(err) {tmp_info2 <- " "}
  )
  tryCatch({tmp_info3 <- tmp_info[3] %>% html_text()},
           error = function(err) {tmp_info3 <- " "}
  )
  tryCatch({tmp_info4 <- tmp_info[4] %>% html_text()},
           error = function(err) {tmp_info4 <- " "}
  )
  
  tmp_data <- paste(tmp_name, tmp_price, tmp_address, tmp_feet, tmp_floor, tmp_type, tmp_status,
                    tmp_info1, tmp_info2, tmp_info3, tmp_info4, sep = "____")
  
  fulldata <- append(tmp_data, fulldata)
  
  n=n+1
  print(n)
  
  if((n %% 500)==0){
    Sys.sleep(300 + rnorm(1,5,2))
  }
}

fulldata <- unlist(fulldata)





