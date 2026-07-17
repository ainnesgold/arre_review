#word cloud by year instead

# install.packages(c("pdftools", "tm", "wordcloud", "RColorBrewer"))

library(pdftools)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Path to folder containing PDFs
pdf_folder <- "PDFs/ByYears/2025-2026"

# Get list of PDF files
pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Extract text from all PDFs (suppress PDF warnings)
all_text <- lapply(pdf_files, function(f) {
  suppressMessages(pdf_text(f))
})

# Combine all pages from all PDFs into one character vector
all_text <- unlist(all_text)

# Collapse into one long text string
combined_text <- paste(all_text, collapse = " ")

# ✅ Use VCorpus (fixes dropped document warnings)
corpus <- VCorpus(VectorSource(combined_text))

# 1. Lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# 2. Remove weird unicode (em dash, etc.)
corpus <- tm_map(corpus, content_transformer(function(x) {
  iconv(x, "UTF-8", "ASCII", sub = " ")
}))

# 3. Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# 4. Custom stopwords
custom_stopwords <- c("study", "results", 
                      "analysis", "figure", "table", "fig", 
                      "doi", "sheries", "shing")

# Combine all stopwords
all_stopwords <- c(stopwords("english"),
                   stopwords("SMART"),
                   custom_stopwords)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, all_stopwords)

# 5. Strip whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Create term-document matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freq <- sort(rowSums(m), decreasing = TRUE)

# ✅ Plot settings to avoid clipping
par(mar = c(0, 0, 0, 0))
set.seed(123)

wordcloud(names(word_freq),
          word_freq,
          max.words = 100,
          scale = c(2.5, 0.5),   # 👈 reduce from 3 → 2.5
          colors = brewer.pal(8, "Dark2"))
