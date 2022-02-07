# Program to identify best starting word for wordle
library(tidyverse)

# First get all words in scrabble dictionary
library(words)
wrds <- as_tibble(words::words) |> filter(word_length == 5)

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

get_letter_freqs <- function(pos, wrds_tb) {
  # merge necessary to fill in gaps if not all letters present in a position
  pos.freqs <- merge(1:26, table(wrds_tb[,pos]), by = 1)
  pos.freqs[,2]
}

# position dependent letter frequencies
letter_freqs <- as.data.frame(sapply(1:5, function(x) t(get_letter_freqs(x, wrds_tb))))
letter_freqs <- letter.freqs / colSums(letter_freqs, na.rm = T)

# Index letter.freqs for each position in each word
wrds$pos_score <- rowSums(sapply(1:ncol(wrds_tb), function(x) letter.freqs[wrds_tb[,x],x]), na.rm=T)

# Get letter freqs and score regardless of position
letter_freqs <- table(wrds_tb)
letter_freqs <- letter_freqs / sum(letter_freqs, na.rm=T)
wrds$score <- apply(wrds_tb, 1, function(x) sum(letter_freqs[x], na.rm=T))

# Combine two priorities to get overall score
wrds$overall_score <- wrds$score * wrds$pos_score

# Show top 10 highest scoring words below
head(wrds[order(-wrds$overall_score),], n=10) |> select(word, overall_score)
