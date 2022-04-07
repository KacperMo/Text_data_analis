# ładowaie bibliotek
library(topicmodels)

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

# analiza ukrytej alokacji Dirichlet'a
n_topics <- 6
lda <- LDA(
  dtm_tf_all,
  n_topics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100, 
    iter = 3000
  )
)
results <- posterior(lda)

# przygotowanie do wykresów
cols <- c("darkslateblue", "darkseagreen", "lightskyblue", "khaki", "darkred", "darkorange")

# prezentacja tematów

for (topic_no in 1:n_topics) {
  par(mai = c(1,2,1,1))
  terms <- tail(sort(results$terms[topic_no,]),20)
  barplot(
    terms, 
    horiz = TRUE,
    las = 1, 
    main = paste("Temat", topic_no),
    xlab = "Prawdopodobieństwo",
    col = cols[topic_no]
  )
}
