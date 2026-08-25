# Word clouds by year

# install.packages(c("pdftools", "tm", "wordcloud", "RColorBrewer"))

library(pdftools)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Path to folder containing PDFs
pdf_folder <- "PDFs/ByYears/2025-2026" # Change based on which years word cloud you want to generate

# Get list of PDF files
pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Extract text from all PDFs
all_text <- lapply(pdf_files, function(f) {
  suppressMessages(pdf_text(f))
})

# Combine all pages from all PDFs into one character vector
all_text <- unlist(all_text)

# Collapse into one long text string
combined_text <- paste(all_text, collapse = " ")
corpus <- VCorpus(VectorSource(combined_text))

# Lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove unicode (em dash, etc.)
corpus <- tm_map(corpus, content_transformer(function(x) {
  iconv(x, "UTF-8", "ASCII", sub = " ")
}))

# Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Custom stopwords
custom_stopwords <- c("study", "results", 
                      "analysis", "figure", "table", "fig", 
                      "doi", "sheries", "shing")

# Combine all stopwords
all_stopwords <- c(stopwords("english"),
                   stopwords("SMART"),
                   custom_stopwords)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, all_stopwords)

# Strip whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Create term-document matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freq <- sort(rowSums(m), decreasing = TRUE)

# Plot settings to avoid clipping
par(mar = c(0, 0, 0, 0))
set.seed(123)

#Generate word cloud
wordcloud(names(word_freq),
          word_freq,
          max.words = 100,
          scale = c(2.5, 0.5),  
          colors = brewer.pal(8, "Dark2"))
