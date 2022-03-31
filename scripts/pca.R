# ładowaie bibliotek


# zmiana katalogu roboczego
work_dir <- "L:\\lato21na22\\PJNS12"
setwd(work_dir)

# lokalizacja katalogu ze skryptami
scripts_dir <- ".\\scripts"

# zdefiniowanie funkcji do tworzenia ścieżek dostępu do plików
create_path <- function(parent, child) 
  paste(parent, child, sep = "\\")

# wczytanie i wykonanie skryptu frequency_matrix.R
source_file <- create_path(
  scripts_dir,
  "frequency_matrix.R"
)
# source(source_file)
eval(
  parse(
    source_file, 
    encoding = "UTF-8"
  )
)

# analiza głównych składowych
pca <- prcomp(dtm_tfidf_2_16)

# przygotowanie danych do wykresu
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  rownames(dtm_tfidf_2_16),
  sep = " - "
)

# legend <- paste(
#   paste(
#     paste(
#       "d",
#       1:length(rownames(dtm_tfidf_2_16)),
#       sep = ""
#     ),
#     rownames(dtm_tfidf_2_16),
#     sep = " - "
#   ),
#   collapse = "\n"
# )

x <- pca$x[,1]
y <- pca$x[,2]

# rysowanie wykresu w przestrzeni 2-wymiarowej
plot(
  x, 
  y, 
  text(
    x,
    y, 
    paste(
      "d",
      1:length(rownames(dtm_tfidf_2_16)),
      sep = ""
    ),
    pos = 4
  ),
  legend(
    "bottom",
    legend,
    cex = 0.6
  )
)
