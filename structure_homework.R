#PSYC 259 Homework 3 - Data Structure
#For full credit, provide answers for at least 8/11 questions

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

install.packages("rvest")

#Load packages
library(tidyverse)
library(lubridate)
library(rvest)


# New URL Doesn't Work; Comment Out

# Scrape the data for the new rolling stone top 500 list
# url <- "https://stuarte.co/2021/2021-full-list-rolling-stones-top-500-songs-of-all-time-updated/"
# rs_new <- url %>% read_html() %>% html_nodes(xpath='//*[@id="post-14376"]/div[2]/div[2]/table') %>% html_table() %>% pluck(1)

# Scrape the data for the old rolling stone top 500 list
# url_old <- "https://www.cs.ubc.ca/~davet/music/list/Best9.html"
# rs_old <- url_old %>% read_html() %>% html_nodes(xpath='/html/body/table[2]') %>% html_table() %>% pluck(1) %>% 
#  select(1, 4, 3, 7) %>% rename(Rank = X1, Artist = X3, Song = X4, Year = X7) %>% filter(Year != "YEAR") 

# If there's a security error, add:
#url %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% read_html()

#OR
load("rs_data.RData")

### Question 1 ---------- 

# Use "full_join" to merge the old and new datasets, rs_new and rs_old,
# by Artist AND Song. Save the results to a dataset called rs_joined_orig
# If the merged worked, each song-artist combination that appears in both
# datasets should now be in a single row with the old/new ranks
# Use the nrow() function to see how many rows of data there are
# In the viewer, take a look at the merge...what kinds of problems are there?
# Why did some of the artist-song fail to match up?

#ANSWER

# join the new and old lists by Artist and Song
rs_joined_orig <- full_join(rs_old, rs_new, by = c('Artist', 'Song'))

# get number of rows in joined df
nrow(rs_joined_orig)

# Some merges didn't work. Perhaps this is because the column data types don't match. Or perhaps because there are some string mismatches in the join by columns


### Question 2 ---------- 

# To clean up the datasets, it would be more efficient to put them into a single data set
# Add a new variable to each dataset called "Source" with value "New" for rs_new and
# "Old" for rs_old. Then use bind_rows to join the two datasets into a single one called rs_all
# You will run into a problem because the old dataset has rank/year as characters instead of integers
# Make Rank and Year into integer variables for rs_old before binding them into rs_all

#ANSWER

# Add Source columns to both the old and new rs dataframes
rs_old <- rs_old %>%
  mutate(Source = 'Old')

rs_new <- rs_new %>%
  mutate(Source = 'New')

# Convert Year and Rank to int in rs_old
rs_old <- rs_old %>%
  mutate(across(c(Rank,Year), as.integer))

# Join the datasets
rs_all <- bind_rows(rs_old, rs_new)


### Question 3 ----------

# The join in Q1 resulted in duplicates because of differences in how the songs and artists names were written
# Use string_remove_all to remove the word "The" from every artist/song (e.g., Beach Boys should match The Beach Boys)
# Use string_replace_all to replace the "&" with the full word "and" from every artist/song
# Then use string_remove_all to remove all punctuation from artists/songs
# Finally, read the documentation for the functions str_to_lower and str_trim
# Use both functions to make all artists/song lowercase and remove any extra spaces

#ANSWER

# remove 'the' from Artist and Song, ignoring case
rs_all <- rs_all %>%
  mutate(across(c(Song,Artist), ~ str_remove_all(., regex("the", ignore_case = T))))

# change '&' to 'and' in Artist and Song
rs_all <- rs_all %>%
  mutate(across(c(Song, Artist), ~ str_replace_all(., '&', 'and')))

# remove punctuation from Artist and Song
rs_all <- rs_all %>%
  mutate(across(c(Song, Artist), ~ str_remove_all(., '[:punct:]')))

# change all text in Artist and Song to lower
rs_all <- rs_all %>%
  mutate(across(c(Song, Artist), ~ str_to_lower(., locale = 'en')))

# remove leading and trailing spaces, as well as double spaces (use squish instead of trim)
rs_all <- rs_all %>%
  mutate(across(c(Song, Artist), ~ str_squish(.)))


### Question 4 ----------

# Now that the data have been cleaned, split rs_all into two datasets, one for old and one for new
# Each dataset should have 500 observations and 5 variables
# Use full_join again to merge the old and new datasets by artist and song, and save it to rs_joined
# Read about the "suffix" argument in full_join, and use it to append _Old and _New to year and rank
# rather than the default (x and y)
# Did the string cleaning improve matches? If so, there should be fewer rows of data (fewer duplicates)
# in the new rs_joined compared to the original. Use nrow to check (there should be 799 rows)

#ANSWER

# Filter the new and old rows into new data frames
rs_new_b <- rs_all %>%
  filter(Source == 'New')

rs_old_b <- rs_all %>%
  filter(Source == 'Old')

# Create suffixes to define which dataset the columns correspond to
suffixes <- c(rs_new_b$Source[1], rs_old_b$Source[1])
suffixes <- paste0('_', suffixes)

# Join the new versions of the data frames
rs_joined <- full_join(rs_new_b, rs_old_b, by = c('Artist', 'Song'), suffix = suffixes)

# rs_joined has 794 rows instead of 799. I think this is because I removed 'the' ignoring case. If you only remove 'The' with that capitalization, then you get 799 rows.


### Question 5 ----------

# Let's clean up rs_joined with the following steps:
  # remove the variable "Source"
  # remove any rows where Rank_New or Rank_Old is NA (so we have only the songs that appeared in both lists)
  # calculate a new variable called "Rank_Change" that subtracts new rank from old rank
  # sort by rank change
# Save those changes to rs_joined
# You should now be able to see how each song moved up/down in rankings between the two lists

#ANSWER

# Remove the Source Columns
rs_joined <- rs_joined %>%
  select(-c(Source_New,Source_Old))

# Remove rows with NA in either Rank column
rs_joined <- rs_joined %>%
  drop_na(Rank_New, Rank_Old)

# Calcualte rank change. Coded as Old-New so that a negative change indicates a decrease in the rankings (higher number on ranking list)
rs_joined <- rs_joined %>%
  mutate(Rank_Change = Rank_Old - Rank_New)


### Question 6 ----------

# Add a new variable to rs_joined that takes the year and turns it into a decade with "s" at the end
# The new variable should be a factor
# 1971 should be 1970s, 1985 should be 1980s, etc.
# Group by decade and summarize the mean rank_change for songs released in each decade (you don't have to save it)
# Which decade improved the most?

#ANSWER



### Question 7 ----------

# Use fct_count to see the number of songs within each decade
# Then use fct_lump to limit decade to 3 levels (plus other), and
# Do fct_count on the lumped factor with the prop argument to see the 
# proportion of songs in each of the top three decades (vs. all the rest)

#ANSWER



### Question 8 ---------- 

# Read the file "top_20.csv" into a tibble called top20
# Release_Date isn't read in correctly as a date
# Use parse_date_time to fix it

#ANSWER


### Question 9 --------

# top20's Style and Value are mixing two different variables into one column
# use pivot_wider to fix the issue so that bpm and key are columns
# overwrite top20 with the pivoted data (there should now be 20 rows!)

#ANSWER



### Question 10 ---------

# Merge in the data from rs_joined to top20 using left_join by artist and song
# The results should be top20 (20 rows of data) with columns added from rs_joined
# Use the "month" function from lubridate to get the release month from the release date
# and add that as a new variable to top 20. 
# It should be a factor - if you get a number back read the help for ?month to see how to get a factor
# Create a new factor called "season" that collapses each set of 3 months into a season "Winter", "Spring", etc.
# Count the number of songs that were released in each season

#ANSWER



### Question 11 ---------

# How many songs in the top 20 were major vs. minor? 
# Create a new factor called "Quality" that is either Major or Minor
# Minor keys contain the lowercase letter "m". If there's no "m", it's Major
# Figure out which is the top-ranked song (from Rank_New) that used a minor key

#ANSWER



