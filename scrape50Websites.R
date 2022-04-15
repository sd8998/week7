install.packages(c("tidyverse",
                   "readr",
                   "rvest",
                   "stringr"))

library("tidyverse")
library("rvest")
library("stringr")
library("readr")

NavTbl <- tibble(
  book = character(),
  pageHdr = character(),
  pageLink = character(),
)



# contains list of comma delimited styled
# book title , base link, search string nav title, search string nav link, search page content

fileName <- "TextMiningLinksEdited.txt"

path <-getwd()
allBooks <- read.csv(paste0(path,"/",fileName))
AllBooksContent <- tibble(book = "", pageHdr = "", secNum = "", text = "")
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Open each book individually
for(i in 1:30) {

    booktitle <- allBooks$book[i]
    baseURL <- trimws(allBooks$url[i])
    webPage <- read_html(baseURL)

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # read nav menu for book
    
    NavHeadings <- webPage %>%
      html_nodes("nav ul.summary li.chapter a") %>%
      html_text()
    NavHdgTbl <- as_tibble(NavHeadings,"text")
    
    NavLinks <- webPage %>%
      html_elements("nav ul.summary li.chapter a") %>%
      html_attr("href")
    NavLinkTbl <- as_tibble(NavLinks,"link")
    
    NavTemp <- tibble(book = booktitle, pageHdr = NavHdgTbl$value, pageLink = NavLinkTbl$value)

    for(j in 1:nrow(NavTemp)) {

      if(grepl("^\\d",NavTemp$pageHdr[j])) {                       # if head begins with a number... remove
        navtest <- strsplit(NavTemp$pageHdr[j], "\\r")[[1]][1]     # splits lengthy header at first line return
        navtest <- strsplit(navtest, "\\d+\\s")[[1]][2]            # splits any header that begins with a number
        NavTemp$pageHdr[j] <- navtest
      }
      if (grepl("#", NavTemp$pageLink[j], fixed = TRUE)) {  # if hashtag in link = TRUE
        NavTemp$pageLink[j] <- ""                           # if hashtag in link, set pageLink to ""
      }
    }
    NavTbl <- NavTemp[!(NavTemp$pageLink==""),]             # delete rows where pageLink is empty
    NavTbl$SecNum <- seq(1,nrow(NavTbl))                    # add section numbers for the book

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # read ALL PAGES of BOOK
    bookContent <- tibble(book = "", pageHdr = "", secNum = "", text = "")

    for(k in 1:nrow(NavTbl)) {
      # read single page of content
      pageContent <- read_html(paste0(baseURL,NavTbl$pageLink[k])) %>%
        html_nodes("section#section-.normal") %>%
        html_text()
      webList <- unlist(str_split(pageContent,"\n"))
      web1 <- as_tibble_col(webList,column_name = "text")
      web0 <- web1[!(trimws(web1$text)==""),]
      web0$book <- NavTbl$book[k] 
      web0$pageHdr <- NavTbl$pageHdr[k]
      web0$secNum <- NavTbl$SecNum[k]
      bookContent <- rbind(bookContent, web0)
    }
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    AllBooksContent <- rbind(AllBooksContent, bookContent)
    print(booktitle)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Load up booktext with all information
write_csv(AllBooksContent, paste0(path,"/","AllBooksContent.csv"))
