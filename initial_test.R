# SET UP =======================================================================
# Load libraries
library(DBI)
library(dplyr)
library(quanteda)
library(readr)

# Connect to DB
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("ndc_soles_dbname"),
                 host = Sys.getenv("ndc_soles_host"), 
                 port = 5432,
                 user = Sys.getenv("ndc_soles_user"),
                 password = Sys.getenv("ndc_soles_password"))

# GET DATA =====================================================================
# Get included data with txt file
dat <- tbl(con, "study_classification") %>% 
  filter(decision == "include") %>%
  select(uid) %>%
  distinct() %>%
  left_join(tbl(con, "unique_citations"), by="uid") %>%
  left_join(tbl(con, "full_texts"), by="doi") %>%
  filter(status == "found") %>%
  filter(grepl("\\.txt$", path)) %>%
  collect()

# Get species regex dictionary and ontology
dictionary <- tbl(con, "pico_dictionary") %>%
  rename(regex_id = id) %>%
  left_join(tbl(con, "pico_ontology"), by="regex_id") %>%
  filter(type == "species") %>%
  filter(regex_id != "9999994") %>%
  collect()

# (TIABKW) CREATE CORPUS =======================================================
# Merge title, abstract, and keywords into one column
dat$tiabkw <- paste(dat$title, dat$abstract, dat$keywords, sep=" ")

# Create corpus
corpus_tiabkw <- quanteda::corpus(dat, text_field = "tiabkw", docid_field = "uid")

# Tokenise
token_tiabkw <- quanteda::tokens(corpus_tiabkw, what = "sentence", 
                                 remove_punct = T,
                                 remove_symbols = T)

# (TIABKW) RUN REGEX ===========================================================
# Run keywords in context function
result_tiabkw <- quanteda::kwic(token_tiabkw, pattern = dictionary$regex,
                                valuetype = "regex", case_insensitive = T) %>%
  rename(regex = pattern)

# Format results
result_tiabkw <- merge(result_tiabkw, dictionary, by = "regex") %>%
  select(uid = docname, name, from, to, pre, keyword, post) %>%
  mutate(method = "kwic_tiabkw")

# Count terms per record
count_tiabkw <- result_tiabkw %>%
  count(uid, name)

# (FULLTEXT) CREATE CORPUS =====================================================
# Set working directory 
setwd("/home/ewilson/SOLES/NDC-SOLES")

# Create subset of dat (so not testing on too many full texts)
dat_sample <- head(dat, 100)

# Create an empty dataframe
dat_fulltext <- data.frame(text = character(), uid = character(), stringsAsFactors = FALSE)

# Loop through each file path
for (i in 1:nrow(dat_sample)) {
  # Read the content of the text file
  text <- readLines(dat_sample$path[i])
  # Create a dataframe with the text and uid
  df <- data.frame(text = paste(text, collapse = "\n"), uid = dat_sample$uid[i], stringsAsFactors = FALSE)
  # Combine the current dataframe with the existing one
  dat_fulltext <- rbind(dat_fulltext, df)
}

# Reset row names
rownames(dat_fulltext) <- NULL

# Create corpus
corpus_fulltext <- quanteda::corpus(dat_fulltext, text_field = "text", docid_field = "uid")

# Tokenise
token_fulltext <- quanteda::tokens(corpus_fulltext, what = "sentence", 
                                 remove_punct = T,
                                 remove_symbols = T)

# (TIABKW) RUN REGEX ===========================================================
# Run keywords in context function
result_fulltext <- quanteda::kwic(token_fulltext, pattern = dictionary$regex,
                                valuetype = "regex", case_insensitive = T) %>%
  rename(regex = pattern)

# Format results
result_fulltext <- merge(result_fulltext, dictionary, by = "regex") %>%
  select(uid = docname, name, from, to, pre, keyword, post) %>%
  mutate(method = "kwic_fulltext")

# Count terms per record
count_fulltext <- result_fulltext %>%
  count(uid, name)

# THOUGHTS =====================================================================
# - Obviously need to work on code more but wanted to start with simple tests
# - TiAbKw appears to be working fine
# - Errors reading in TXT files (e.g. 43: In readLines(dat$path[i]) : 
#   incomplete final line found on 'full_texts/10.1073%2Fpnas.1102233108.txt')

# Next steps:
# - Want to work out what is happening with txt files
# - Want to compare these results with AutoAnnotation
# - Want to test other pdftotext conversion
# - Want to compare outputs when we have both PDF and XML etc.