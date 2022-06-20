#Loading packages

library('rvest')
library('purrr')
library('dplyr')
library('pander')
library('stringr')

#Specifying the url of the website to be scraped, we have choosen year 2020

#this link shows all the articles for year 2020(page1)

url <- 'https://gsejournal.biomedcentral.com/articles?query=&volume=52&searchType=&tab=keyword'


#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.c-listing__title a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Look at the title
head(title_data)

#this link shows all the articles for year 2020(page2)
url2<-'https://gsejournal.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=52&page=2'
#Reading the HTML code from the website
webpage2 <- read_html(url2)

#Using CSS selectors to scrape the title section
title_data_html2 <- html_nodes(webpage2,'.c-listing__title a')

#Converting the title data to text
title_data2 <- html_text(title_data_html2)

# create a vector name title to show the article titles from both pages 
title=c(title_data,title_data2)

##  extract article links (page1)
name = webpage %>% html_nodes("h1") %>% html_text()
article_links = webpage %>% html_nodes(".c-listing__title a") %>%
  html_attr("href") %>% paste("https://gsejournal.biomedcentral.com", ., sep="")
publish_date=webpage %>% html_nodes(".c-meta__item+ .c-meta__item") %>% html_text()%>% paste(collapse = ",")

##  extract the fields  for each article: 


# extract the authors 
get_author = function(article_link) {
  article_page = read_html(article_link)
  article_author = article_page %>% html_nodes(".c-article-author-affiliation__authors-list") %>%
    html_text() %>% paste(collapse = ",")
  return(article_author)
}
author = sapply(article_links, FUN = get_author, USE.NAMES = FALSE)

# extract author affiliations

get_affiliations = function(article_link) {
  article_page = read_html(article_link)
  author_affiliations = article_page %>% html_nodes(".c-article-author-affiliation__address") %>%
    html_text() %>% paste(collapse = ",")
  return(author_affiliations)
}
author_affiliations = sapply(article_links, FUN = get_affiliations, USE.NAMES = FALSE)

#extract correspondence author

get_correspondence = function(article_link) {
  article_page = read_html(article_link)
  correspondence_author = article_page %>% html_nodes("#corresponding-author-list") %>%
    html_text() %>% paste(collapse = ",")
  return(correspondence_author)
}
correspondence_author = sapply(article_links, FUN = get_correspondence, USE.NAMES = FALSE)

# extract abstract

get_abstract = function(article_link) {
  article_page = read_html(article_link)
  abstract = article_page %>% html_nodes("#Abs1-content") %>%
    html_text() %>% paste(collapse = ",")
  return(abstract)
}
abstract = sapply(article_links, FUN = get_abstract, USE.NAMES = FALSE)


#extract full paper

get_fullpaper = function(article_link) {
  article_page = read_html(article_link)
  full_paper = article_page %>% html_nodes("article") %>%
    html_text() %>% paste(collapse = ",")
  return(full_paper)
}
full_paper = sapply(article_links, FUN = get_fullpaper, USE.NAMES = FALSE)

## keywords and email field is missing 


#extract the arctile links (page2)
article_links2 = webpage2 %>% html_nodes(".c-listing__title a") %>%
  html_attr("href") %>% paste("https://gsejournal.biomedcentral.com", ., sep="")

#extract field 1: authors, for each article
get_author2 = function(article_link2) {
  article_page2 = read_html(article_link2)
  article_author2 = article_page2 %>% html_nodes(".c-article-author-affiliation__authors-list") %>%
    html_text() %>% paste(collapse = ",")
  return(article_author2)
}
author2 = sapply(article_links2, FUN = get_author2, USE.NAMES = FALSE)

#extract field 2: author_affiliations, for each article

get_affiliations2 = function(article_link2) {
  article_page2 = read_html(article_link2)
  author_affiliations2 = article_page2 %>% html_nodes(".c-article-author-affiliation__address") %>%
    html_text() %>% paste(collapse = ",")
  return(author_affiliations2)
}
author_affiliations2 = sapply(article_links2, FUN = get_affiliations2, USE.NAMES = FALSE)

#extract field 3: correspondence_author, for each article 

get_correspondence2 = function(article_link2) {
  article_page2 = read_html(article_link2)
  correspondence_author2 = article_page2 %>% html_nodes("#corresponding-author-list") %>%
    html_text() %>% paste(collapse = ",")
  return(correspondence_author2)
}
correspondence_author2 = sapply(article_links2, FUN = get_correspondence2, USE.NAMES = FALSE)

#extract field 4: abstract, for each article

get_abstract2 = function(article_link2) {
  article_page2 = read_html(article_link2)
  abstract2 = article_page2 %>% html_nodes("#Abs1-content") %>%
    html_text() %>% paste(collapse = ",")
  return(abstract2)
}
abstract2 = sapply(article_links2, FUN = get_abstract2, USE.NAMES = FALSE)


#extract field 5: full paper, for each article 

get_fullpaper2 = function(article_link2) {
  article_page2 = read_html(article_link2)
  full_paper2 = article_page2 %>% html_nodes("article") %>%
    html_text() %>% paste(collapse = ",")
  return(full_paper2)
}
full_paper2 = sapply(article_links2, FUN = get_fullpaper2, USE.NAMES = FALSE)

#extract field 6: publish date, for each article 

get_date2 = function(article_link2) {
  article_page2 = read_html(article_link2)
  publish_date2 = article_page2 %>% html_nodes(".c-bibliographic-information__list-item~ .c-bibliographic-information__list-item+ .c-bibliographic-information__list-item time") %>%
    html_text() %>% paste(collapse = ",")
  return(publish_date2)
}
publish_date2 = sapply(article_links2, FUN = get_date2, USE.NAMES = FALSE)

#put the data from both pages in a vector for each field 

Title=c(title_data,title_data2)
Authors=c(author,author2)
Author_Affiliations=c(author_affiliations,author_affiliations2)
Correspondence_Author=c(correspondence_author,correspondence_author2)
Publish_Date=c(publish_date,publish_date2)
Abstract=c(abstract,abstract2)
Full_Paper=c(full_paper,full_paper2)

