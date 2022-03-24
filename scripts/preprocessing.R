# ładowaie bibliotek
library(tm)
library(hunspell)

# zmiana katalogu roboczego
work_dir <- "D:\\KW\\PJNS12"
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
    "UTF-8",
    "*.txt"
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

# zdefiniowanie funkcji do usuwania pojedynczych znaków
remove_char <- content_transformer(
  function(text, char) gsub(char, "", text)
)
corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
corpus <- tm_map(corpus, remove_char, intToUtf8(190))
corpus <- tm_map(corpus, remove_char, "”")
corpus <- tm_map(corpus, remove_char, "„")


# zdefiniowanie funkcji do usuwania rozszerzeń z nazw plików
cut_extensions <- function(document, ext){
  meta(document, "id") <- gsub(
    paste("\\.", ext, "$", sep = ""),
    "",
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cut_extensions, "txt")

# zdefiniowanie funkcji usuwającej podział na akapity w tekście
paste_paragraphs <- function(text){
  paste(text, collapse = " ")
}

corpus <- tm_map(corpus, content_transformer(paste_paragraphs))
corpus <- tm_map(corpus, stripWhitespace)

#zdefiniowanie funkcji do lematyzacji
polish <- dictionary("pl_PL")
lemmatize <- function(text){
  text <- corpus[[1]]$content
  parsed_text <- hunspell_parse(text, dict = polish)
  lemmatized_text <- hunspell_stem(parsed_text, dict = polish)
  for (i in 1:length(lemmatized_text)) {
    if (length(lemmatized_text[i]) == 0) 
      lemmatized_text[i] <- parsed_text[[1]][i]
    if (length(lemmatized_text[i]) >  1) 
      lemmatized_text[i] <- lemmatized_text[[i]][1]
  }
  new_text <- paste(lemmatized_text, collapse = " ")
  return(new_text)
}


