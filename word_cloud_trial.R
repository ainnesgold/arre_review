#install.packages(c("pdftools", "tm", "wordcloud"))
library(pdftools)
library(tm)
library(wordcloud)

# Path to folder containing PDFs
pdf_folder <- "PDFs"

# Get list of PDF files
pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Extract text from all PDFs
all_text <- lapply(pdf_files, pdf_text)

# Combine all pages from all PDFs into one character vector
all_text <- unlist(all_text)

# Collapse into one long text string
combined_text <- paste(all_text, collapse = " ")

corpus <- Corpus(VectorSource(combined_text))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# 2. Remove weird unicode (em dash, etc.)
corpus <- tm_map(corpus, content_transformer(function(x) {
  iconv(x, "UTF-8", "ASCII", sub = " ")
}))

# Add custom stopwords 
custom_stopwords <- c("study", "data", "model", "results", 
                      "analysis", "figure", "table", "fig", "doi")

corpus <- tm_map(corpus, removeWords,
                 c(stopwords("english"),
                   stopwords("SMART"),
                   custom_stopwords))

corpus <- tm_map(corpus, stripWhitespace)

dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freq <- sort(rowSums(m), decreasing = TRUE)

par(mar = c(0, 0, 0, 0))  # remove margins
set.seed(123)

wordcloud(names(word_freq),
          word_freq,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))



