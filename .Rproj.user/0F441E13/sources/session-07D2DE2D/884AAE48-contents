pacman::p_load(tidyverse, here, gt)


data_raw <- read_csv(here('data', 'scopus18384-2.csv')) %>% 
  janitor::clean_names()

cols_needed <- c(
  'title',
  'year',
  'authors',
  'cited_by',
  'author_keywords',
  'index_keywords',
  'authors_with_affiliations',
  'affiliations',
  'abstract'
)

data_ided_na <- data_raw %>% 
  # colnames()
  select(all_of(cols_needed)) %>% 
  rowid_to_column() 

##  THERE ARE 447 RECORDS WITH MISSING IKW AND AKW.
##  I WILL DROP THESE RECORDS FOR CONVIENIENCE FOR NOW.
##  I TRIED WITH NLP IMPUTATION, SCOPUS API, AND CROSSREFF, USELESS EFFORT.

data_ided <- data_ided_na %>% 
  filter(!is.na(author_keywords) & !is.na(index_keywords))

# data_raw %>% 
#   filter(is.na(author_keywords)) %>% 
#   select(doi) %>% 
#   write_csv(here('data', 'missing_kw.csv'))
  


authors_affiliations <- data_ided %>% 
  select(rowid, year, authors_with_affiliations) %>% 
  # Split authors on semicolon and optional whitespace
  mutate(authors_with_affiliations = strsplit(as.character(authors_with_affiliations), ";\\s*")) %>% 
  unnest(authors_with_affiliations) %>% 
  mutate(author = str_extract(authors_with_affiliations, "^[^,]+"),
         # Extract the country, which is after the last comma
         country = str_extract(authors_with_affiliations, "[^,]+$"),
         # Remove the author and country from the affiliation
         affiliation = str_remove(authors_with_affiliations, "^[^,]+,\\s*"),
         affiliation = str_remove(affiliation, ",\\s*[^,]+$"))


author_keywords<- data_ided %>% 
  select(rowid, year, author_keywords) %>% 
  mutate(author_keywords = strsplit(as.character(author_keywords), "[;,]\\s*")) %>% 
  unnest(author_keywords) 


index_keywords<- data_ided %>% 
  select(rowid, year, index_keywords) %>% View()
  mutate(index_keywords = strsplit(as.character(index_keywords), "[;,]\\s*")) %>% 
  unnest(index_keywords) 


  
