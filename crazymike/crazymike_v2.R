library(httr)
library(rvest)
library(xml2)
library(doParallel)
library(gsubfn)
library(magrittr)
#=========================================================================
# Find the total page from urls :
n = 150
while(TRUE){
  tmpurl = paste0("https://crazymike.tw/index/ch-5/page-",as.character(n))
  res <- GET(tmpurl) %>% content("parse")
  xpath = '//*[@id="wBody"]'
  tmptext <- res %>% html_node(xpath=xpath) %>% html_children() %>% '[['(5) %>% html_children() %>% html_children()
  count = length(tmptext)
  if(count<36){
    if(count!=0){
      final_page = n
      break
    }
    else{
      final_page = n - 1
      break}
  }
  n = n + 1
}

#=========================================================================
# page-1 = 64 , use [4]
targeturl <- "https://crazymike.tw/index/ch-5/page-1"
res <- GET(targeturl) %>% content("parse")
xpath = '//*[@id="wBody"]'
text1 <- res %>% html_node(xpath=xpath) %>% html_children() %>% '[['(4) %>% 
  html_children() %>% html_children()
url1 <- unlist(lapply(as.character(text1), strapplyc, 'href=\"(.*)\" target='))

#-------------------------------------------------------------------------
# page-2 to page-173 = 36 , use [5] , use parallel
num_seq = 2:final_page
targeturl <- paste0("https://crazymike.tw/index/ch-5/page-",num_seq)

core <- detectCores()-1
Split_url <- split(targeturl, 1:core)
(cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel

ws <- foreach(i = 1:length(Split_url), .combine = 'c', .packages = c("rvest", "magrittr", "xml2","httr","gsubfn")) %dopar% {
  # set function for lapply :
  webpage_parser <- function(x){
    res_tmp <- GET(x) %>% content("parse")
    xpath = '//*[@id="wBody"]'
    res_tmp <- res_tmp %>% html_node(xpath=xpath) %>% html_children() %>% '[['(5) %>% 
      html_children() %>% html_children()
    unlist(lapply(as.character(res_tmp), strapplyc, 'href=\"(.*)\" target='))
  }
  
  text <- unlist(lapply(Split_url[[i]], function(x) webpage_parser(x)))
  text
}

stopCluster(cl)

# 2016-09-26 = 6222 units

#=========================================================================
# Create function for each part of loop :
webpage_parser <- function(x){
  
  (cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel
  
  testdata <- foreach(i = 1:length(x), .combine = 'c', .packages = c("rvest","magrittr","xml2","httr","gsubfn")) %dopar% {
    
    list1 <- list()
    list2 <- list()
    xpath1='//*[@id="item_name"]'
    xpath2='//*[@id="item_price"]/div[2]'
    
    for (each in x[[i]]){
      
      res_tmp <- GET(each) %>% content("parse")
      item_temp <- res_tmp %>% html_node(xpath=xpath1) %>% html_text()
      
      list1 <- append(item_temp,list1)
      
      price_temp <- res_tmp %>% html_node(xpath=xpath2) %>% html_text()
      price_temp <- strsplit(price_temp, "[^0-9]+")[[1]][2]
      
      list2 <- append(price_temp,list2)
    }
    
    item_name <- unlist(list1)
    item_price <- unlist(list2)
    
    paste(item_name, item_price, sep='______')
  }
  stopCluster(cl)
  
  return(testdata)
}
#-------------------------------------------------------------------------
# extract data(name & price) from all downloaded html files :
# Split all urls into 3 parts :
starttime <- Sys.time()

ws <- split(ws, 1:3)
core <- detectCores() - 1

totallist <- list()

Split_url <- split(ws[[1]], 1:core)
testdata <- webpage_parser(Split_url)
totallist <- append(totallist,testdata)
Sys.sleep(20)

Split_url <- split(ws[[2]], 1:core)
testdata <- webpage_parser(Split_url)
totallist <- append(totallist,testdata)
Sys.sleep(20)

Split_url <- split(ws[[3]], 1:core)
testdata <- webpage_parser(Split_url)
totallist <- append(totallist,testdata)
Sys.sleep(5)

runtime <- Sys.time() - starttime
runtime
#=========================================================================
# process data :
library(magrittr)
library(dplyr)
library(tidyr)
library(RODBC)
totallist <- unlist(totallist)
# use RODBC connect to Mysql
conn <- odbcConnect("bigmoumou", uid = "bigmoumou", pwd = "a0911136226")
# data.frame
ws_df <- data.frame(date=starttime, finaldata=totallist)
ws_df <- ws_df %>% separate(finaldata, c("item","price"), sep="______")
# process col "price" and "date"
ws_df$price <- as.numeric(gsub("[^0-9]", "", ws_df$price))
ws_df$date <- as.character(ws_df$date)
sqlSave(conn, ws_df, tablename='crazymike', rownames = FALSE, append = TRUE)
# disconnect
close(conn)
#==========================================================================
#==========================================================================
# retrieve data
# conn <- odbcConnect("bigmoumou", uid = "bigmoumou", pwd = "a0911136226")
# res <- sqlFetch(conn, "livemarket")
# close(conn)
#==========================================================================
#==========================================================================
# library(taskscheduleR)
# taskscheduleR::taskscheduler_create(taskname="livemkt_daily" ,
#                                     rscript = "C:\\Users\\PIAU\\Desktop\\scrape_paraleel_livemkt_static.R",
#                                     schedule = "DAILY", days = "*", starttime = "06:00", startdate = format(Sys.Date(), "%Y/%m/%d"), debug = TRUE)
#==========================================================================
#==========================================================================

