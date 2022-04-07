# ładowaie bibliotek
library(lsa)

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

# analiza ukrytych wymiarów semantycznych
# dekompozycja według wartości osobliwych 
lsa <- lsa(tdm_tf_2_16_m)

# przygotowanie danych do wykresu
coord_docs <- lsa$dk%*%diag(lsa$sk)
coord_terms <- lsa$tk%*%diag(lsa$sk)

terms_importance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
important_terms <- names(tail(sort(terms_importance),30))
own_terms <- c("eustachy", "harry", "ron", "hermiona", "dumbledore",
               "umbridge", "syriusz", "łucja", "zuzanna", "piotr",
               "edmund", "aslana", "narnii", "bell", "edward",
               "jacob", "wampir", "czarownica", "czarodziej")

coord_important_terms <- coord_terms[important_terms,]
coord_own_terms <- coord_terms[own_terms,]

coord_plot_terms <- coord_own_terms

legend <- paste(
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_m)),
    sep = ""
  ),
  colnames(tdm_tf_2_16_m),
  sep = " - "
)

x1 <- coord_docs[,1]
y1 <- coord_docs[,2]

x2 <- coord_plot_terms[,1]
y2 <- coord_plot_terms[,2]

# rysowanie wykresu w przestrzeni 2-wymiarowej
plot(
  x1, 
  y1, 
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
# xlim = c(-25,5),
# ylim = c(0,20),
  col = "darkmagenta"
)
text(
  x1,
  y1, 
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_m)),
    sep = ""
  ),
  pos = 4,
  col = "darkmagenta"
)
points(
  x2, 
  y2,
  pch = 2,
  col = "darkslateblue"
)
text(
  x2,
  y2, 
  rownames(coord_plot_terms),
  pos = 4,
  col = "darkslateblue"
)
legend(
  "topleft",
  legend,
  cex = 0.5,
  text.col = "darkmagenta"
)

# zapis wykresu do pliku .png
lsa_file <- create_path(
  output_dir,
  "lsa.png"
)
png(lsa_file)
plot(
  x1, 
  y1, 
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  # xlim = c(-25,5),
  # ylim = c(0,20),
  col = "darkmagenta"
)
text(
  x1,
  y1, 
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_m)),
    sep = ""
  ),
  pos = 4,
  col = "darkmagenta"
)
points(
  x2, 
  y2,
  pch = 2,
  col = "darkslateblue"
)
text(
  x2,
  y2, 
  rownames(coord_plot_terms),
  pos = 4,
  col = "darkslateblue"
)
legend(
  "topleft",
  legend,
  cex = 0.5,
  text.col = "darkmagenta"
)
dev.off()
