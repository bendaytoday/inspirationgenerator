library(shiny)
library(tidyverse)
library(DT)
library(xml2)

file <- 'bookmarks_firefox.html'
a <- read_html(file)

# Get folder names
folders <- a %>% html_nodes("dt h3") %>%
  html_text()

# # Put into list
# links <- bind_rows(lapply(xml_attrs(html_nodes(a, "dt a")), 
#                           function(x) data.frame(as.list(x), stringsAsFactors = FALSE))) %>%
#   select(href) %>%
#   rename('link' = href) %>%
#   filter(str_detect(link, 'http'))

# names <- bind_rows(lapply(html_text(html_nodes(a, "dl a")), 
#                           function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))

# Get bookmark names
names <- a %>% html_nodes("dl a") %>%
  html_text()

# Get links
links <- a %>% html_nodes("dt a") %>%
  html_attr("href")

# Make a dataframe
df <- data.frame(cbind(names, links)) %>%
  filter(str_detect(links, 'http'))

# Return a random link
y <- slice(df, sample(1:nrow(df), 1))
as.character(y$names)
as.character(y$links)
