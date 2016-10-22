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
# Download by parallel : 7 thread <= 20 min
core <- detectCores()-1

ws <- split(ws, 1:3)
#-------------------------------------------------------------------------
DownloadHTML <- function(x){
  
  (cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel
  
  ws <- foreach(i = 1:length(x), .combine = 'c', .packages = c("rvest","magrittr","xml2","httr","gsubfn")) %dopar% {
    for (each in x[[i]]){
      filename = paste0('C:\\Users\\cby2\\Desktop\\R\\webscraping\\crazymike\\temp\\', substring(each, nchar(each)-4, nchar(each)), '.html')
      download.file(each, filename)
    }
  }
  stopCluster(cl)
}
#-------------------------------------------------------------------------
Split_url <- split(ws[[1]], 1:core)
DownloadHTML(Split_url)
Sys.sleep(60)

Split_url <- split(ws[[2]], 1:core)
DownloadHTML(Split_url)
Sys.sleep(60)

Split_url <- split(ws[[3]], 1:core)
DownloadHTML(Split_url)
Sys.sleep(10)

#=========================================================================
# extract data(name & price) from all downloaded html files :

starttime <- Sys.time()

total_filename <- list.files(path = "C:\\Users\\cby2\\Desktop\\R\\webscraping\\crazymike\\temp", full.names = TRUE, recursive = TRUE)

core <- detectCores()-1
Split_url <- split(total_filename, 1:core)
(cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel

list1=list()
list2=list()

finaldata <- foreach(i = 1:length(Split_url), .combine = 'c', .packages = c("rvest","magrittr","xml2","httr","gsubfn")) %dopar% {
  webpage_parser <- function(x,xpath){
    read_temp <- read_html(x)
    read_temp %>% html_node(xpath=xpath) %>% html_text()
  }  
  
  xpath1='//*[@id="item_name"]'
  xpath2='//*[@id="item_price"]/div[2]'
  for (each in Split_url[[i]]){
    item_temp <- webpage_parser(each, xpath = xpath1)
    list1 <- append(item_temp,list1)
    
    price_temp <- webpage_parser(each, xpath = xpath2)
    price_temp <- strsplit(price_temp, "[^0-9]+")[[1]][2]
    list2 <- append(price_temp,list2)
  }
  
  item_name <- unlist(list1)
  item_price <- unlist(list2)
  
  paste(item_name, item_price, sep='______') # 6 * "_"
}

stopCluster(cl)

runtime <- Sys.time() - starttime
runtime
#=========================================================================
# Remove all downloaded html files :
do.call(file.remove, list(list.files("C:\\Users\\cby2\\Desktop\\R\\webscraping\\crazymike\\temp", full.names = TRUE)))

#=========================================================================
# process data :
library(magrittr)
library(dplyr)
library(tidyr)
library(RODBC)
# use RODBC connect to Mysql
conn <- odbcConnect("bigmoumou", uid = "bigmoumou", pwd = "a0911136226")
# data.frame
ws_df <- data.frame(date=starttime, finaldata=finaldata)
ws_df <- ws_df %>% separate(finaldata, c("item","price"), sep="______")
# process col "price" & "buyers"
ws_df$price  <- as.numeric(gsub("[^0-9]", "", ws_df$price))

sqlSave(conn, ws_df, tablename='livemarket', rownames = FALSE, append = TRUE)
# disconnect
close(conn)


