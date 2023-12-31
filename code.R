install.packages("rvest")
install.packages("data.table")
install.packages("rlist")


library(tidyverse)
library(rvest)
library(data.table)
library(rlist)

setwd("C:/Users/Matt/Documents/Rprojects/scrape_yelp_rvest")

# look up the review you want to scrape and change the reviewers sort to by date / either newest or oldest

df_final <- list()

pageNums <- page |> 
  html_elements(xpath = "//div[@aria-label = 'Pagination navigation']") |> 
  html_text() |> 
  str_extract('of \\d+') |> 
  str_remove('of ') |> 
  as.numeric()
pageNums

pageSequence <- seq(from = 0, to = (pageNums*10)-10, by=10)

# create a loop

for (i in pageSequence) {
  
  url <- sprintf("https://www.yelp.com/biz/ruby-memorial-hospital-morgantown?osq=ruby+memorial&start=%d&sort_by=date_desc", i)
  
  page <- read_html(url)
  # Sections of review - using rvest in combination with looking at inspect feature of the browser to find unique features of the sections
  
  # usernames
  
  usernames  <- page |> 
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") |> 
    html_elements(xpath = ".//a[starts-with(@href, '/user_details')]") |> 
    html_text()
  usernames
  
  # locations
  
  locations <- page |> 
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") |> 
    html_elements(xpath = ".//span[@class= ' css-qgunke']") |> 
    html_text() %>%
    .[.!="Location"]
  locations
  
  # review
  
  comments <- page |> 
    html_elements(xpath = "//li[starts-with(@class, ' margin-b5__09f24__pTvws')]") |> 
    html_elements(xpath = "(.//p[starts-with(@class, 'comment')])[1]") |> 
    html_text()
  comments
  
  # ratings
  
  ratings <- page |> 
    html_elements(xpath = "//li[starts-with(@class, ' margin-b5__09f24__pTvws')]") |> 
    html_elements(xpath = "(.//div[contains(@aria-label, 'star rating')])[1]") |> 
    html_attr("aria-label") |> 
    str_remove_all(" star rating") |> 
    as.numeric()
  ratings
  
  #dates
  
  the_dates <- page |> 
    html_elements(xpath = "//li[starts-with(@class, ' margin-b5__09f24__pTvws')]") |> 
    html_elements(xpath = "(.//span[@class = ' css-chan6m'])[1]") |> 
    html_text() 
  the_dates
  
  # extra info
  
  extra_info <- page |> 
    html_elements(xpath = "//li[starts-with(@class, ' margin-b5__09f24__pTvws')]") |> 
    html_elements(xpath = ".//button[@type = 'submit']") |> 
    html_text() %>%
    .[.!= ""] %>%
    .[.!= "Read more"] 
  extra_info
  
  extra_info_extract <- function(ei, txt) {
    str_extract(ei, paste0(txt, ".*")) %>%
      .[!is.na(.)]  %>% 
      str_extract("\\d+")  %>% 
      str_replace_na("0")  %>% 
      as.numeric
  }
  
  useful <- extra_info_extract(extra_info, "Useful")
  useful              
  
  funny <- extra_info_extract(extra_info, "Funny")
  funny
  
  cool <- extra_info_extract(extra_info, "Cool")
  cool
  
  # combine it into a data frame
  
  df_new <- list(username = usernames, 
                 dates = the_dates,
                 location = locations, 
                 rating = ratings,
                 comment = comments,
                 useful = useful,
                 funny = funny,
                 coll = cool)
  
  df_new_table <- as.data.frame(df_new)
  
  df_final <- rbindlist(list(df_final, df_new_table))
  
  # code pauses
  
  Sys.sleep(sample(c(15, 25), 1))
  
}

write_csv(df_final, "ruby_yelp.csv")


