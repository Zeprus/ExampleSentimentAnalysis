if (!require("quanteda")) {
    install.packages("quanteda")
    library("quanteda")
}
if (!require("tidyverse")) {
    install.packages("tidyverse")
    library("tidyverse")
}
if (!require("scales")) {
    install.packages("scales")
    library("scales")
}
if (!require("lubridate")) {
    install.packages("lubridate")
    library("lubridate")
}

load("docs.RData")
load("docs_meta.RData")

keywords <- c("sanctions", "sanction", "embargos", "embargo")
infile <- "./Uberarbeitet_Russland_gegen_Rebellen.csv"
filenames <- read.csv(file = infile, sep = ";", fileEncoding = "latin1")[[7]]
wordbuffer <- 30

# column ids
country_index <- which(colnames(meta_speeches) == "country")
filename_index <- which(colnames(meta_speeches) == "filename")
topic_index <- which(colnames(meta_speeches) == "topic")
topic2_index <- which(colnames(meta_speeches) == "topic2")
subtopic_index <- which(colnames(meta_speeches) == "subtopic")
agendaitem1_index <- which(colnames(meta_speeches) == "agenda_item1")
agendaitem2_index <- which(colnames(meta_speeches) == "agenda_item2")
agendaitem3_index <- which(colnames(meta_speeches) == "agenda_item3")
year_index <- which(colnames(meta_speeches) == "year")

# get speech text by filename
get_text <- function(raw_docs, input) {
    return(raw_docs[which(raw_docs[which(colnames(raw_docs) == "doc_id")] == input), which(colnames(raw_docs) == "text")])
}

# Find stuff
for (i in 1:nrow(meta_speeches)) {
    if (meta_speeches[i, filename_index] %in% filenames) {
        text <- gsub("[^[:alnum:] ]", " ", get_text(raw_docs, meta_speeches[i, filename_index]))
        kwic <- kwic(tokens(text), pattern = keywords, window = wordbuffer, case_insensitive = TRUE)
        passages <- paste(kwic$pre, kwic$keyword, kwic$post)
        pass_tokens <- tokens(passages)
        dfm <- dfm(pass_tokens)
        lookup <- dfm_lookup(dfm, dictionary = data_dictionary_LSD2015)
        weight <- dfm_weight(lookup, scheme = "prop")
        cat(paste("Year: ", meta_speeches[i, year_index], " Topic: ", meta_speeches[i, topic_index], " Agendaitem2: ", meta_speeches[i, agendaitem2_index], " Filename: ", meta_speeches[i, filename_index], sep = ""))
        cat("\n")
        print(passages)
        print(weight, max_ndoc = 100)
        cat("\n\n")
    }
}
