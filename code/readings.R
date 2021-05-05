
readings <- function() {
  system("pandoc data/.readings.bib -s -f biblatex -t csljson -o data/readings.json")
}
