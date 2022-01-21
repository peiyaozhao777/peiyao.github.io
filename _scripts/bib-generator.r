library("rcrossref")
library("yaml")
library("anytime")
library(stringr)
setwd("../papers/_posts/")
dois <- c("10.1103/PhysRevLett.104.070402",
          # "10.1093/jamia/ocaa033",
          # "10.1177/0361198119853553",
          # "10.1007/s11116-020-10106-y",
          # "10.1088/1748-9326/ab22c7",
          # "10.1016/j.jth.2015.08.006",
          # "10.1007/s10479-016-2358-2",
          # "10.1007/s11067-018-9387-0",
          # "10.1016/j.cor.2015.02.010",
          # "10.1016/j.tra.2021.09.013",
          "10.1038/s41598-021-01522-w"
          )
for(ii in 1:length(dois)) {

  ## get doi reference with rcrossref:
  ref_md <- cr_cn(dois = dois[ii], format = "bibtex")
  #ref_md <- as.yaml(ref_md)
  ref_md <- unlist(strsplit(ref_md, "\n"))

  
  # layout  
  ref_layout <- "paper"
  
  # title
  ref_title <- str_extract(grep("title =", ref_md, value = TRUE), "\\{(.*)\\}")
  ref_title <- gsub("\\{|\\}", "", ref_title)
  title_list <- unlist(strsplit(ref_title, " "))
  ref_title <- paste0("title: ", ref_title)
  
  # date
  ref_year <- str_extract(grep("year =", ref_md, value = TRUE), "\\{(.*)\\}")
  ref_year <- gsub("\\{|\\}", "", ref_year)
  ref_year <- paste0("date: ", ref_year)


  # authors 
  authors <- str_extract(grep("author =", ref_md, value = TRUE), "\\{(.*)\\}")
  authors <- gsub("\\{|\\}", "", authors)
  authors <- gsub(" and", ",", authors)
  author_list <- unlist(strsplit(authors, ","))
  authors <- paste0("authors: ", authors)
  
  # reference
  first_author_lastname <- unlist(strsplit(author_list[1], " "))[2]
  if (length(author_list) > 2) {
    ref_ref = paste0(first_author_lastname, " et al.")
  } else if (length(author_list) == 2) {
    second_author_lastname <- unlist(strsplit(author_list[2], " "))[2]
    ref_ref = paste0(first_author_lastname, " and ", second_author_lastname)
  } else {
    ref_ref = first_author_lastname
  }
  ref_journal <- str_extract(grep("journal =", ref_md, value = TRUE), "\\{(.*)\\}")
  ref_journal <- gsub("\\{|\\}", "", ref_journal)
  ref_ref = paste0(ref_ref, " ", ref_year, ". ", ref_journal)
  
  # citation: journal/volume/number/pages
  ref_volume <- gsub("\\{|\\}", "", str_extract(grep("volume =", ref_md, value = TRUE), "\\{(.*)\\}"))
  ref_number <- gsub("\\{|\\}", "", str_extract(grep("number =", ref_md, value = TRUE), "\\{(.*)\\}"))
  
  ref_pages <- gsub("\\{|\\}", "", str_extract(grep("pages =", ref_md, value = TRUE), "\\{(.*)\\}"))
  ref_pages <- gsub("--", "-", ref_pages)
  
  if (length(ref_number) != 0) {
    vol_num_pages = paste0(ref_volume, "(", ref_number, ")")
  } else {
    vol_num_pages = ref_volume
  }
  if (length(ref_pages) != 0) {
    vol_num_pages = paste0(vol_num_pages, ":", ref_pages)
  }
  ref_cit <- paste0("journal: ", ref_journal, " ", vol_num_pages, ".")
 
  
  # doi
  doi <- str_extract(grep("doi =", ref_md, value = TRUE), "\\{(.*)\\}")
  doi <- gsub("\\{|\\}", "", doi)
  doi <- paste0("doi: ", doi)
  
  # abstract
  ref_abstract <- cr_abstract(doi = dois[ii])
  ref_abstract <- gsub("Abstract", "", ref_abstract, fixed=TRUE)
  
  ## make hugo-finite theme compliant *.md files from reference:
  ref_hugo_md <- c(ref_layout, ref_title, authors, ref_year, ref_ref, ref_cit, ref_volume, doi)
  if (length(ref_abstract) > 0) {
    ref_hugo_md <- c("---", ref_hugo_md, "---", "# Abstract", ref_abstract )  
  } else {
    ref_hugo_md <- c("---", ref_hugo_md, "---")  
  }
  
  ## format filename
  ref_month <- gsub("\\{|\\}", "", str_extract(grep("month =", ref_md, value = TRUE), "\\{(.*)\\}"))
  if (length(ref_month) > 0 ) {
    ref_month <- str_which(ref_month, fixed(month.abb, ignore_case = TRUE))
    if (ref_month < 10) {
      ref_month <- paste0("0", ref_month)
    }
  } else {
    ref_month = "00"
  }
  
  ref_day <- gsub("\\{|\\}", "", str_extract(grep("month =", ref_md, value = TRUE), "\\{(.*)\\}"))
  if (length(ref_day) > 0 ) {
    if (length(ref_day) < 10 ) {
      ref_day = paste0("0", ref_day)
    }
  } else {
    ref_day = "00"
  }
  
  stopwords <- c("in", "for", "and", "a", "an", "the", "of", "on", "this")
  title_list <- title_list[!title_list %in% stopwords]
  title_list <- title_list[1:3]
  title_list <- paste(title_list, collapse = " ")
  
  ref_filename = tolower(paste0(ref_year, ref_month, ref_day, first_author_lastname, sep = "-"))
  if (!file.exists(paste0("../papers/_posts/", ref_filename, ".md"))) {
    write.table(ref_hugo_md, file = paste0(ref_filename, ".md"), #"./content/publications/",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    print(paste(ii, ":", dois[ii], "---", ref_filename))
  }

}
