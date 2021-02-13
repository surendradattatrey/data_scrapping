rm(list=ls())

require("rvest")||install.packages("rvest")
require("XML")||install.packages("XML")
require("gsubfn")||install.packages("gsubfn")

library(rvest)
library(XML)

page = read_html("https://www.ratebeer.com/beer/top-50/")
first_column = html_nodes(page,"#tablecontent td:nth-child(2) a")

test = html_nodes(page,"#tablecontent td:nth-child(1)")
score = as.numeric (gsub("\\s+","",html_text(html_nodes(page,"#tablecontent td:nth-child(5) font"))))

fif_column = html_text(html_nodes(page,"b"))

# xmlTreeParse(first_column[[1]])

name = html_text(first_column)


url = paste0("https://www.ratebeer.com/",html_attr(first_column,"href"))
score = html_text(html_nodes(page,"#tablecontent b"))

count = html_text(html_nodes(page,"font"))
style = html_text(html_nodes(page,"td:nth-child(6) a")) 
brewed_by = html_text(html_nodes(page," #_brand4 span"))
rating = html_text(html_nodes(page,"#_ratingCount8"))
weighted_avg = html_text(html_nodes(page," strong span:nth-child(1)"))
seasonal = html_text(html_nodes(page," a+ big strong"))
est_cal = html_text(html_nodes(page,".stats-container big:nth-child(6)"))
ABV = html_text(html_nodes(page,"abbr+ big strong"))

df = data.frame(name, url, score,count, style, stringsAsFactors = F)
head(df,4)

df$count = as.numeric(gsub("\\s","",df$count))
head(df)

df1 = df[df$count>500,]
head(df1)
# now for each beer extract the data
t = Sys.time()

out = list()

df=df1

nrow(df)

for (row in 1:nrow(df)){
  u = df$url[row]
  detail.df = NULL
  for (j in 1:5) {
    u1 = paste0(u,"1/",j,"/")
    page1 = read_html(u1)
    for (k in c(seq(1,22,5), seq(27,50,5))){
      row1 = html_text(
        html_nodes(page1,
                   paste0("#container > div.row.columns-container > div.col-sm-8 > div.reviews-container > div > div > div:nth-child(",
                          k,") > div:nth-child(2)")))
      
      row2 = html_text(html_nodes(page1,paste0("#container > div.row.columns-container > div.col-sm-8 > div.reviews-container > div > div > div:nth-child(",k,") > strong")))
      row3 = html_text(html_nodes(page1,paste0("#container > div.row.columns-container > div.col-sm-8 > div.reviews-container > div > div > small:nth-child(",k+1,")")))
      row4 = html_text(html_nodes(page1,paste0("#container > div.row.columns-container > div.col-sm-8 > div.reviews-container > div > div > div:nth-child(",k+3,")")))
      
      aroma = gsub("AROMA|/10| ","",
                   strapplyc(row2,"AROMA [0-9]/10|AROMA [0-9][0-9]/10")[[1]])
      
      appearance = gsub("APPEARANCE|/5| ","",strapplyc(row2,"APPEARANCE [0-5]/5")[[1]])
      taste = gsub("TASTE|/10| ","",strapplyc(row2,"TASTE [0-9]/10|TASTE [0-9][0-9]/10")[[1]])
      palate = gsub("PALATE|/5| ","",strapplyc(row2,"PALATE [0-5]/5")[[1]])
      overall = gsub("OVERALL|/20| ","",strapplyc(row2,"OVERALL [0-9]/20|OVERALL [0-9][0-9]/20")[[1]])
      
      review = row4
      rating = as.numeric(row1)
      location = row3
      
      dft = data.frame(rating,aroma,appearance,taste,palate,overall,review,location, stringsAsFactors = F)
      
      detail.df = rbind(detail.df,dft)
    }
  }
  
  detail.df$url = u
  detail.df$Name = df$name[row]
  
  out[[df$name[row]]] = detail.df
  
}
Sys.time() - t