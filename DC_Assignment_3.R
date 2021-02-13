rm(list=ls())

library(xml2)
library(rvest)
library(XML)

page <- read_html("https://www.ratebeer.com/beer/top-50/")

name <- html_text(html_nodes(page,"#tablecontent td:nth-child(2) a"))
url <- paste0("https://www.ratebeer.com", html_attr(html_nodes(page,"#tablecontent td:nth-child(2) a"), "href"))
count <- gsub("\\s+", "", html_text(html_nodes(page,"#tablecontent td:nth-child(5) font")))
style <- html_text(html_nodes(page,"#tablecontent td:nth-child(6) a"))

temp_df <- data.frame(name, url, count, style, stringsAsFactors = F)

temp_df$count <- as.numeric(temp_df$count)

Table1 <- subset(temp_df, count>500)

head(Table1$url)

tempdf_1 <- c("Brewed_by", "Weighted_Avg", "Season", "Est_Calories", "ABV", "Commercial_Description")  
Table1[,tempdf_1] <- ""

Table2 <- NULL

for (i in 1:nrow(Table1)) {
  page1 <- read_html(Table1$url[i]) 
  xyz <- page1 %>% 
    html_nodes("[class=stats-container]") %>%
    html_text()
  Table1$Brewed_by[i] <- html_text(html_nodes(page1,"#_brand4 span"))
  Table1$Weighted_Avg[i] <- gsub("\\s+", "", strsplit(strsplit(xyz, "WEIGHTED AVG: ")[[1]][2], " ")[[1]][1])
  Table1$Season[i] <- gsub("\\s+", "", strsplit(strsplit(xyz, "SEASONAL: ")[[1]][2], " ")[[1]][1])
  Table1$Est_Calories[i] <- gsub("\\s+", "", strsplit(strsplit(xyz, "EST. CALORIES: ")[[1]][2], " ")[[1]][1])
  Table1$ABV[i] <- gsub("\\s+", "", strsplit(strsplit(xyz, "ABV: ")[[1]][2], " ")[[1]][1])
  Table1$Commercial_Description[i] <- gsub("\r\n |^\\s+ | \\s+", "", html_text(html_nodes(page1,".text-wrapper span")))
  
  for (j in 1:3) {
    review_page <- read_html(paste0(Table1$url[i], "1/", j, "/")) 
    
    RATING <- html_text(html_nodes(review_page,".reviews-container div+ div , .reviews-container > .reviews-container strong"))
    
    ds <- html_text(html_nodes(review_page,".reviews-container strong"))
    ds <- as.data.frame(ds)  
    ds$ds <- as.character(ds$ds)
    ds$ds <- gsub("\\s+", "", ds$ds)
    
    AROMA <- gsub("AROMA", "", regmatches(ds$ds, regexpr("AROMA[0-9]*/10", ds$ds)))
    APPEARANCE <- gsub("APPEARANCE", "", regmatches(ds$ds, regexpr("APPEARANCE[0-9]*/5", ds$ds)))
    TASTE <- gsub("TASTE", "", regmatches(ds$ds, regexpr("TASTE[0-9]*/10", ds$ds)))
    PALATE <- gsub("PALATE", "", regmatches(ds$ds, regexpr("PALATE[0-9]*/5", ds$ds)))
    OVERALL <- gsub("OVERALL", "", regmatches(ds$ds, regexpr("OVERALL[0-9]*/20", ds$ds)))
    REVIEW <- html_text(html_nodes(review_page,".reviews-container div:nth-child(4), div:nth-child(9), div:nth-child(14), div:nth-child(19), div:nth-child(24), div:nth-child(30), div:nth-child(35), div:nth-child(40), div:nth-child(45), div:nth-child(50)"))[1:10]
    
    DATE_LOC <- strsplit(html_text(html_nodes(review_page,".reviews-container div+ small")), " - ")
    DATE_LOC <- as.data.frame(DATE_LOC)
    DATE_LOC <- as.data.frame(t(DATE_LOC))
    DATE_LOC <- subset(DATE_LOC, select = -c(V1))
    rownames(DATE_LOC) <- NULL
    colnames(DATE_LOC)<- c("LOCATION","DATE")
    
    RATING <- as.data.frame(RATING)
    AROMA <- as.data.frame(AROMA)
    APPEARANCE <- as.data.frame(APPEARANCE)
    TASTE <- as.data.frame(TASTE)
    PALATE <- as.data.frame(PALATE)
    OVERALL <- as.data.frame(OVERALL)
    REVIEW <- as.data.frame(REVIEW)
    
    temp_table2 <- NULL
    temp_table2 <- cbind(RATING, AROMA, APPEARANCE, TASTE, PALATE, OVERALL, REVIEW, DATE_LOC)
    
    Table2 <- rbind(Table2, temp_table2)
    
  }  
}

remove(list=c("AROMA", "APPEARANCE", "TASTE", "PALATE", "OVERALL", "REVIEW", "temp_table2", "DATE_LOC", "ds", "RATING", "temp_df"))

