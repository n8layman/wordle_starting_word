# Program to identify best starting word for wordle
# https://www.powerlanguage.co.uk/wordle/

library(tidyverse)
library(words)

# First get all words in a dictionary.
# Uncomment the one you want to use below

# Scrabble dictionary
wrds <- as_tibble(words::words) |> filter(word_length == 5)

# Wordle dictionary
# source: https://docs.google.com/spreadsheets/d/1-M0RIVVZqbeh0mZacdAsJyBrLuEmhKUhNaVAI-7pr2Y/edit#gid=0
#wrds <- read_csv2("wordle_words.csv", col_names=c("word")) |> mutate(word_length = str_length(word))

# Split words into individual letters.
wrds_tb <- t(sapply(wrds$word, function(x) str_split(x, "")[[1]]))

# Convert letters to their number positions along alphabet
wrds_tb <- as.data.frame(apply(wrds_tb, 2, function(x) match(x, letters)))

# Penalize repeated letters by setting all but their first occurrence to NA
na_repeats <- function(x) {
  x[setdiff(1:length(x), match(unique(x), x))] <- NA
  x
}
wrds_tb <- t(apply(wrds_tb, 1, function(x) na_repeats(x)))

# Position dependent letter frequency indexing function
get_letter_freqs <- function(pos, wrds_tb) {
  # merge necessary to fill in gaps if not all letters present in a position
  pos.freqs <- merge(1:26, table(wrds_tb[,pos]), by = 1, all=T)
  pos.freqs[,2]
}

# Position dependent letter frequencies
letter_freqs <- sapply(1:ncol(wrds_tb), function(x) t(get_letter_freqs(x, wrds_tb)))
letter_freqs <- letter_freqs / colSums(letter_freqs, na.rm = T)

# Position dependent word score
wrds$pos_score <- rowSums(sapply(1:ncol(wrds_tb), function(x) letter_freqs[wrds_tb[,x],x]), na.rm=T)

# Position independent letter frequencies
letter_freqs <- table(wrds_tb)
letter_freqs <- letter_freqs / sum(letter_freqs, na.rm=T)

# Position independent word score
wrds$score <- apply(wrds_tb, 1, function(x) sum(letter_freqs[x], na.rm=T))

# Combine the two priorities to get overall score
# In reality these two priorities likely have an ideal
# weighting. Here I've just assumend equal importance.
wrds$overall_score <- wrds$score * wrds$pos_score

# Show top 10 highest scoring words below
wrds <- wrds[order(-wrds$overall_score),] |> mutate(id = 1:n())
wrds
