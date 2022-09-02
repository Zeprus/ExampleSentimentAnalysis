library("quanteda")

load("docs.RData")
load("docs_meta.RData")

args <- commandArgs(trailingOnly = TRUE)

# DEBUG
args[1] <- "-country=Russian Federation"
args[2] <- "-keywords=sanction,embargo"
args[3] <- "-agendaitem1=Africa"
args[4] <- "-count=8"
# args[5] <- "-blacklist=embargo"
args[5] <- "-infile=./Uberarbeitet_Russland_gegen_Staaten.csv"

# vars
skip_topic <- FALSE
skip_text <- FALSE
skip_subtopic <- FALSE
agendaitem1 <- ""
country <- ""
keywords <- ""
blacklist <- ""
infile <- ""
filenames <- ""
wordbuffer <- 0
passages <- c()

# Arguments
cat("\n")
for (i in 1:length(args)) {
    if (tolower(args[i]) == "-skiptopic") {
        skip_topic <- TRUE
    }
    if (tolower(args[i]) == "-skiptext") {
        skip_text <- TRUE
    }
    if (tolower(args[i]) == "-skipsubtopic") {
        skip_subtopic <- TRUE
    }
    if (startsWith(tolower(args[i]), "-country=")) {
        country <- strsplit(args[i], "=")[[1]][2]
        cat("Country:\n")
        print(country)
    }
    if (startsWith(tolower(args[i]), "-keywords=")) {
        keywords_split <- strsplit(args[i], "=")[[1]][2]
        keywords <- strsplit(keywords_split[], ",")[[1]]
        cat("Keywords:\n")
        print(keywords)
    }
    if (startsWith(tolower(args[i]), "-agendaitem1=")) {
        agendaitem1 <- strsplit(args[i], "=")[[1]][2]
        cat("Agendaitem1:\n")
        print(agendaitem1)
    }
    if (startsWith(tolower(args[i]), "-blacklist=")) {
        blacklist_split <- strsplit(args[i], "=")[[1]][2]
        blacklist <- strsplit(blacklist_split[], ",")[[1]]
        cat("Blacklist:\n")
        print(blacklist)
    }
    if (startsWith(tolower(args[i]), "-infile=")) {
        infile <- strsplit(args[i], "=")[[1]][2]
        cat("In-File:\n")
        print(infile)
        filenames <- read.csv(file = infile, sep = ";", fileEncoding = "latin1")[[7]]
    }
    if (startsWith(tolower(args[i]), "-count=")) {
        wordbuffer <- as.numeric(strsplit(args[i], "=")[[1]][2])
        cat("Count:\n")
        print(wordbuffer)
    }
}
cat("\n")

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

# cat("ls:\n")
# print(ls())
# cat("\nmeta_speeches:\n")
# print(colnames(meta_speeches))
# cat("\nraw_docs\n")
# print(colnames(raw_docs))
# cat("\n")

# Check if input contains keywords
contains_words <- function(input, words) {
    ret <- FALSE
    # print(length(keywords))
    for (i in 1:length(words)) {
        # print(keywords[i])
        if (grepl(words[i], input, ignore.case = TRUE)) {
            ret <- TRUE
        }
    }
    return(ret)
}

# get speech text by filename
get_text <- function(raw_docs, input) {
    return(raw_docs[which(raw_docs[which(colnames(raw_docs) == "doc_id")] == input), which(colnames(raw_docs) == "text")])
}

# get text passage by length and filter
get_passages <- function(text, patterns, amount, storage) {
    words <- strsplit(gsub("[^[:alnum:] ]", " ", text), " +")[[1]]

    # create result vector
    result <- list()

    # iterate over all patterns
    for (pattern in patterns) {
        # empty index vector
        indices <- vector()

        # iterate over words and look for matches
        indices <- grepl(pattern, words)

        # create passages vector
        passages <- list()

        # run variable
        passagenum <- 1

        # iterate over word indices to concat passage
        for (i in 1:length(indices)) {
            if (indices[i]) {
                passage <- ""
                start_index <- 0
                end_index <- 0
                if (i - amount >= 0) {
                    start_index <- i - (amount)
                } else {
                    start_index <- 0
                }
                if (i + amount <= length(words)) {
                    end_index <- i + (amount)
                } else {
                    end_index <- length(words)
                }
                for (i in start_index:end_index) {
                    passage <- trimws(paste(passage, words[i], sep = " "))
                }
                passages <- append(passages, passage)
                passagenum <- passagenum + 1
            }
        }
        # print(passages)
        result <- append(result, passages)
    }
    return(result)
}

# Find stuff
count <- 0
# print(filenames)
for (i in 1:nrow(meta_speeches)) {
    if (
        # if infile exists and contains speech
        (infile == "" || (meta_speeches[i, filename_index] %in% filenames)) &&
            # if is by county and agendaitem fits
            meta_speeches[i, country_index] == country && (agendaitem1 == "" || (!is.na(meta_speeches[i, agendaitem1_index]) && meta_speeches[i, agendaitem1_index] == agendaitem1)) &&
            # keyword filter
            (((!skip_topic && contains_words(meta_speeches[i, topic_index], keywords))) ||
                (!skip_subtopic && contains_words(meta_speeches[i, subtopic_index], keywords)) ||
                (!skip_text && contains_words(get_text(raw_docs, meta_speeches[i, filename_index]), keywords))) &&
            # blacklist filter
            (is.na(blacklist) || blacklist == "" || (((skip_topic || !contains_words(meta_speeches[i, topic_index], blacklist))) &&
                (skip_subtopic || !contains_words(meta_speeches[i, subtopic_index], blacklist)) &&
                (skip_text || !contains_words(get_text(raw_docs, meta_speeches[i, filename_index]), blacklist))))
    ) {
        count <- count + 1
        cat(paste(meta_speeches[i, year_index], meta_speeches[i, country_index], meta_speeches[i, topic_index], meta_speeches[i, agendaitem1_index], meta_speeches[i, agendaitem2_index], meta_speeches[i, agendaitem3_index], meta_speeches[i, filename_index], sep = ","))
        cat("\n")
        print(get_passages(get_text(raw_docs, meta_speeches[i, filename_index]), keywords, wordbuffer, passages))
    }
}
print(count)
