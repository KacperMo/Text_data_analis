# ładowaie bibliotek
library(tm)

# zmiana katalogu roboczego
work_dir <- "L:\\lato21na22\\PJNS12"
setwd(work_dir)

# definicja lokalizacji katalogów funkcyjnych
input_dir <- ".\\data"
scripts_dir <- ".\\scripts"
output_dir <- ".\\results"
workspaces_dir <- ".\\workspaces"

# utworzenie katalogów wyjściowych
dir.create(output_dir, showWarnings = F)
dir.create(workspaces_dir, showWarnings = F)

# zdefiniowanie funkcji do tworzenia ścieżek dostępu do plików
create_path <- function(parent, child) 
  paste(parent, child, sep = "\\")

# utworzenie korpusu dokumentów
corpus_dir <- create_path(
  input_dir,
  "Literatura - streszczenia - oryginal"
)
corpus <- VCorpus(
  DirSource(
    corpus_dir,
    "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

# wstępne przetwarzanie dokumentów
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
stoplist_file <- create_path(
  input_dir,
  "stopwords_pl.txt"
)
stoplist <- readLines(stoplist_file, encoding = "UTF-8")

corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)
