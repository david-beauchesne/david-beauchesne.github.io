
mypubs <- function(my_orcid = "0000-0002-3590-8161") {
  my_orcid = "0000-0002-3590-8161"
  filename <- "data/.publications.bib"
  if(file.exists(filename)) file.remove(filename)
  pubs <- rorcid::orcid_works(my_orcid)
  my_dois <- unique(rorcid::identifiers(rorcid::works(my_orcid)))
  pubs <- rcrossref::cr_cn(dois = my_dois, format = "bibtex")
  invisible(lapply(pubs, write, filename, append = TRUE))
  system("pandoc data/.publications.bib -s -f biblatex -t csljson -o data/publications.json")
}
