#! /usr/bin/Rscript

# TODO: need a script to automatically format and correct decklists (apostrophes, extra spaces, etc)

library(dplyr)
library(jsonlite)

# Parameters
#setwd('/home/jason/Fun/Magic the Gathering/JumpStart cards')
deck_directory="Decklists"
all_cards_file="oracle-cards-20210709210504.name_only.txt"
deck_size=20
multiples_okay = c("Plains", "Island", "Swamp", "Mountain", "Forest",
                   "Thriving Heath", "Thriving Isle", "Thriving Moor", "Thriving Bluff", "Thriving Grove")
cat("Checking Jumpstart decks in directory", deck_directory, "\n")

# Load data
cat("\tLoading data\n")
decklists = list.files(path=deck_directory, pattern=".csv", full.names = TRUE)
decks = lapply(decklists, read.csv)
data = do.call(rbind, decks)
all_cards = scan(all_cards_file, what=character(), sep='\n') # List of all cards

###########
# Correct common issues
###########

data$Card = gsub(data$Card, pattern="’", repl="'") # Frequent apostrophe error


#################
# Check deck size
#################
size= data %>% group_by(Deck) %>% summarize(count= sum(Qty))
right_size = size$count == deck_size
if(all(right_size)){
  cat("\tPASS: All decks contain", deck_size,"cards\n")
}else{
  cat("\tERROR: ",sum(right_size), "decks contain", deck_size,"cards. Incorect sizes:\n")
  wrong_size = size[!right_size,]
  size_output = paste(wrong_size$Deck, " (", wrong_size$count, " cards)", sep="")
  cat("\t\t", paste(size_output, collapse="\n\t\t"), "\n")
}



#################
# Check card names correct
#################

deck_cards = unique(data$Card)
isfound = deck_cards %in% all_cards
not_found = sort(deck_cards[!isfound])
if(length(not_found) == 0){
  cat("\tPASS: All cards found in Scryfall database\n")
}else{
  cat("\tERROR: Unable to locate", length(not_found)  ,"cards in Scryfall database:\n")
  
  # Decks the multiple card are in
  in_decks = sapply(not_found, function(c){
    decks = subset(data, data$Card==c)$Deck
    paste(decks, collapse=", ")
  })
  
  # Print out error
  notfound_output = paste("\t\t", not_found, " (", in_decks,")", sep="")
  cat(paste(notfound_output, collapse="\n"), "\n")
}



#################
# Check card uniqueness
#################

cards = table(data$Card)
multiples = cards[cards>1]
problems = multiples[!names(multiples) %in% multiples_okay]
if(length(problems) == 0){
  cat("\tPASS: All cards are singleton (except basic and thriving lands)\n")
}else{
  cat("\tERROR: Multiple copies found of some cards:\n")
  
  # Decks the multiple card are in
  in_decks = lapply(names(problems), function(p){
    mult_decks = subset(data, data$Card==p)$Deck
    paste(mult_decks, collapse=", ")
  })
  
  # Print out error
  multiples_output = paste("\t\t", names(problems), " (", problems, " instances: ", in_decks,")", sep="")
  cat(paste(multiples_output, collapse="\n"), "\n")
}