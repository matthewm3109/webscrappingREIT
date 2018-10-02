#get data from library 
library(rvest)
library(stringr)
##rvest is a library that makes it easy to harvest data from HTML webpages. Its not good 
##with Javascript-based applets. RSelenium is much more versitile but harder to use. 

#-----extract the names of all Canadian REITS from Wikipedia 

ReturnREITNames <- function()
{
  URL = "https://en.wikipedia.org/wiki/List_of_REITs_in_Canada" 
  ReitXML <- read_html(URL)
  ReitNameXML <- html_nodes(ReitXML, "ul:nth-child(19) li , ul:nth-child(17) li, ul:nth-child(15) li, ul:nth-child(13) li, ul:nth-child(11) li, ul:nth-child(9) li, ul:nth-child(7) li, ul:nth-child(5) li")
  ReitNameElement <- as.data.frame(as.character(html_text(ReitNameXML)))
  return(ReitNameElement) 
}

#-----extract the ticker from names 
ExtractTicker <- function(c)
{
  d = vector()
  for( i in 1:nrow(c))
  {
    #convert from factor to character 
    e=as.character(c[i,])
    end_matrix <- as.data.frame(str_locate_all(e,".UN"))
    if (nrow(end_matrix) == 0) 
    {
      d[i] = "N/A"
      next
    }
    end <- end_matrix[nrow(end_matrix),2]
    s1 <- substr(e,str_locate(e,"TSX:")[1]+5,end)
    s2<-str_replace(s1,"[.]","-")
    d[i] <- str_c(s2,"-T")
    
  }
  return(cbind(c,d))
  
}
#-----scrape fundementals from Globe and Mail 
ScrapeFundementals <- function(f)
{ 
  Dividend <- vector()
  DebtEquity <- vector()
  for (i in 1:nrow(f)) 
  {
    g <- as.character(f[i,2])
    URL2 <- str_c("https://www.theglobeandmail.com/investing/markets/stocks/",g,"/")
    errormessage = try(a2 <- read_html(URL2))
    if ((as.character(class(errormessage)[1]) == "try-error") || (as.character(f[i,2]) == "N/A"))
    {
      next()
    }
    b2 <- html_nodes(a2,":nth-child(7) .bc-percent-change")
    b3 <- html_nodes(a2,"#fundamentals :nth-child(2) :nth-child(10) barchart-field")
    c2 <- html_attrs(b2)[[1]][5]
    DebtEquity[i] <- as.numeric(html_attrs(b3)[[1]][4])
    Dividend[i] <- as.numeric(substr(c2,1,nchar(c2)-1))
  }

  return(cbind(f,Dividend,DebtEquity))
}
#-----main method 
ReitInfo <- ScrapeFundementals(ExtractTicker(ReturnREITNames()))

