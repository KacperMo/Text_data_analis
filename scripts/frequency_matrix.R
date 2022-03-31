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
  "Literatura - streszczenia - przetworzone"
)
corpus <- VCorpus(
  DirSource(
    corpus_dir,
    "CP1250",
    #"*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

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

# utworzenie macierzy częstości
tdm_tf_all <- TermDocumentMatrix(corpus)
tdm_bin_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdm_tfidf_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdm_tf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdm_tfidf_2_16 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

dtm_tf_all <- DocumentTermMatrix(corpus)
dtm_bin_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
dtm_tfidf_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
dtm_tf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tfidf_2_16 <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

# konwersja macierzy rzadkich do postaci macierzy klasycznych
tdm_tf_all_m <- as.matrix(tdm_tf_all)
tdm_bin_all_m <- as.matrix(tdm_bin_all)
tdm_tfidf_all_m <- as.matrix(tdm_tfidf_all)
tdm_tf_2_16_m <- as.matrix(tdm_tf_2_16)
tdm_tfidf_2_16_m <- as.matrix(tdm_tfidf_2_16)
dtm_tf_all_m <- as.matrix(dtm_tf_all)
dtm_bin_all_m <- as.matrix(dtm_bin_all)
dtm_tfidf_all_m <- as.matrix(dtm_tfidf_all)
dtm_tf_2_16_m <- as.matrix(dtm_tf_2_16)
dtm_tfidf_2_16_m <- as.matrix(dtm_tfidf_2_16)

# zapisanie macierzy częstości do pliku
# matrix_file <- create_path(
#   output_dir,
#   "tdm_tf_all.csv"
# )
# write.table(
#   tdm_tf_all_m,
#   matrix_file,
#   sep = ";",
#   dec = ",",
#   col.names = NA
# )
