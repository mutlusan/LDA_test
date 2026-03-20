# LDA Analysis with R

# Load required libraries
library(readr)  # For CSV reading
library(readxl) # For Excel reading
library(topicmodels) # For LDA analysis
library(tm) # For text mining
library(SnowballC) # For stemming
library(ggplot2) # For visualization
library(tidytext) # For text manipulation
 
# Function to load data
load_data <- function(file_path) {
  if (grepl("\\.csv$", file_path)) {
    data <- read_csv(file_path)
  } else if (grepl("\\.xlsx$", file_path)) {
    data <- read_excel(file_path)
  } else {
    stop("Unsupported file format. Please provide a CSV or Excel file.")
  }
  return(data)
}

# Function to preprocess text
preprocess_text <- function(text_column) {
  corpus <- Corpus(VectorSource(text_column))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(DocumentTermMatrix(corpus))
}

# Function to train LDA model
train_lda <- function(dtm, num_topics = 5) {
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  return(lda_model)
}

# Function to visualize LDA topics
visualize_topics <- function(lda_model, dtm) {
  topics <- tidy(lda_model, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    arrange(topic, -beta);

  ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(title = "Top Terms in Each Topic", x = "Term", y = "Beta")
}

# Main script
file_path <- readline(prompt = "Please enter the file path (CSV or Excel): ")
data <- load_data(file_path)

if (!"text" %in% colnames(data)) {
  stop("The dataset must contain a 'text' column.")
}

# Preprocess the text
dtm <- preprocess_text(data$text)

# Train the LDA model
num_topics <- as.integer(readline(prompt = "Please enter the number of topics: "))
lda_model <- train_lda(dtm, num_topics)

# Visualize the topics
visualize_topics(lda_model, dtm)