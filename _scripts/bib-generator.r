library("rcrossref")
library("yaml")
library("anytime")
library(stringr)
setwd("../papers/_posts/")
dois <- c(
          "10.1103/PhysRevLett.104.070402",
          "10.1093/jamia/ocaa033",
          "10.1177/0361198119853553",
          "10.1007/s11116-020-10106-y",
          "10.1088/1748-9326/ab22c7",
          "10.1016/j.jth.2015.08.006",
          "10.1007/s10479-016-2358-2",
          "10.1007/s11067-018-9387-0",
          "10.1016/j.cor.2015.02.010",
          "10.1016/j.tra.2021.09.013",
          "10.1038/s41598-021-01522-w",
          "10.1016/j.tra.2020.06.013"
          )
for(ii in 1:length(dois)) {

  ## get doi reference with rcrossref:
  ref_md <- cr_cn(dois = dois[ii], format = "bibtex")
  #ref_md <- as.yaml(ref_md)
  ref_md <- unlist(strsplit(ref_md, "\n"))

  
  # layout  
  ref_layout <- "layout: paper"
  
  # title
  title <- str_extract(grep("title =", ref_md, value = TRUE), "\\{(.*)\\}")
  title <- gsub("\\{|\\}", "", title)
  title_list <- unlist(strsplit(title, " "))
  ref_title <- paste0("title: ", title)
  
  # date
  year <- gsub(",", "", unlist(strsplit(grep("year =", ref_md, value = TRUE), " "))[3])
  ref_year <- paste0("year: ", year)


  # authors 
  authors <- str_extract(grep("author =", ref_md, value = TRUE), "\\{(.*)\\}")
  authors <- gsub("\\{|\\}", "", authors)
  authors <- gsub(" and", ",", authors)
  author_list <- unlist(strsplit(authors, ","))
  ref_authors <- paste0("authors: ", authors)
  
  # reference
  first_author_lastname <- tail(unlist(strsplit(author_list[1], " ")), n=1)
  if (length(author_list) > 2) {
    cit = paste0(first_author_lastname, " et al.")
  } else if (length(author_list) == 2) {
    second_author_lastname <- tail(unlist(strsplit(author_list[2], " "))[2], n=1)
    cit = paste0(first_author_lastname, " and ", second_author_lastname)
  } else {
    cit = first_author_lastname
  }
  journal <- str_extract(grep("journal =", ref_md, value = TRUE), "\\{(.*)\\}")
  journal <- gsub("\\{|\\}", "", journal)
  ref_ref = paste0("ref: ", cit, " ", year, ". ", journal)
  
  # citation: journal/volume/number/pages
  volume <- gsub("\\{|\\}", "", str_extract(grep("volume =", ref_md, value = TRUE), "\\{(.*)\\}"))
  number <- gsub("\\{|\\}", "", str_extract(grep("number =", ref_md, value = TRUE), "\\{(.*)\\}"))
  
  pages <- gsub("\\{|\\}", "", str_extract(grep("pages =", ref_md, value = TRUE), "\\{(.*)\\}"))
  pages <- gsub("--", "-", pages)
  
  if (length(number) != 0) {
    vol_num_pages = paste0(volume, "(", number, ")")
  } else {
    vol_num_pages = volume
  }
  if (length(pages) != 0) {
    vol_num_pages = paste0(vol_num_pages, ":", pages)
  }
  ref_journal <- paste0("journal: ", journal, " ", vol_num_pages, ".")
 
  ref_volume <- paste0("volume: ", volume)
  
  # doi
  doi <- str_extract(grep("doi =", ref_md, value = TRUE), "\\{(.*)\\}")
  doi <- gsub("\\{|\\}", "", doi)
  ref_doi <- paste0("doi: ", doi)
 
  ## make theme compliant *.md files from reference:
  ref_theme_md <- c(ref_layout, ref_title, ref_authors, ref_year, ref_ref, ref_journal, ref_volume, "pdf:", ref_doi, "github:")
  
  # abstract
  get_abstract <- function(doi) {
    abstract = tryCatch({
      cr_abstract(doi)
      message("Successfully extracted abstract")
    },
    error = function(e) {
      message("Abstract not found. Original error message:")
      message(paste(e))
      return(character(0))
    },
    finally = {
      message("Contents of reference markdown completed")
    })
    return(abstract)
  }
  
  abstract <- get_abstract(dois[ii])
  if (length(abstract) > 0) {
    abstract <- gsub("Abstract", "", abstract, fixed = TRUE)
    ref_theme_md <- c("---", ref_theme_md, "---", "# Abstract", abstract)
  } else {
    ref_theme_md <- c("---", ref_theme_md, "---") 
  }
  
  ## format filename
  month <- gsub("\\{|\\}", "", str_extract(grep("month =", ref_md, value = TRUE), "\\{(.*)\\}"))
  if (length(month) > 0 ) {
    month <- str_which(month, fixed(month.abb, ignore_case = TRUE))
    if (month < 10) {
      month <- paste0("0", month)
    }
  } else {
    month = "00"
  }
  
  day <- gsub("\\{|\\}", "", str_extract(grep("day =", ref_md, value = TRUE), "\\{(.*)\\}"))
  if (length(day) > 0 ) {
    if (length(day) < 10 ) {
      day = paste0("0", day)
    }
  } else {
    day = "00"
  }
  
  stopwords <- c("in", "for", "and", "a", "an", "the", "of", "on", "this", "to", "who", "whom", "whose", "why", "how", "where", "is", "my", "from")
  title_list <- tolower(title_list)
  title_list <- title_list[!title_list %in% stopwords]
  title_list <- title_list[1:3]
  title_list <- gsub("[[:punct:]]", "", title_list) # remove punctuation marks
  title_list <- paste(title_list, collapse = "-")
  
  
  filename = tolower(paste(year, month, day, first_author_lastname, title_list, sep = "-"))
  if (!file.exists(paste0("../papers/_posts/", filename, ".md"))) {
    write.table(ref_theme_md, file = paste0(filename, ".md"), #"./content/publications/",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    print(paste(ii, ":", dois[ii], "---", filename))
  }

}
