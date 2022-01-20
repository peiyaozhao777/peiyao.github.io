library("rcrossref")
library("yaml")
library("anytime")
library(stringr)
setwd("../papers/_posts/")
dois <- c("10.1103/PhysRevLett.104.070402",
          "10.1093/jamia/ocaa033",
          "10.1177/0361198119853553",
          "10.1007/s11116-020-10106-y",
          "10.1088/1748-9326/ab22c7",
          "10.1016/j.jth.2015.08.006",
          "10.1007/s10479-016-2358-2",
          "10.1007/s11067-018-9387-0",
          "10.1016/j.cor.2015.02.010",
          "10.1016/j.tra.2021.09.013",
          "10.1038/s41598-021-01522-w"
          )
for(ii in 1:length(dois)) {

  ## get doi reference with rcrossref:
  ref_md <- cr_cn(dois = dois[ii], format = "bibtex")
  #ref_md <- as.yaml(ref_md)
  ref_md <- unlist(strsplit(ref_md, "\n"))

  ## change and construct yaml fields
  #url_doi <- gsub("URL:", "url_doi:", grep("^URL: ", ref_md, value = TRUE))
  doi <- str_extract(grep("doi =", ref_md, value = TRUE), "\\{(.*)\\}")
  doi <- gsub("\\{|\\}", "", doi)
  doi <- paste0("doi: ", doi)
  # authors 
  authors <- str_extract(grep("author =", ref_md, value = TRUE), "\\{(.*)\\}")
  authors <- gsub("{", "", authors)
  authors <- gsub("}", "", authors)
  authors <- gsub(" and", ",", authors)
  authors <- c("authors:", authors)

  # if (length(grep("published-online:", ref_md)) >0 ) {
  #   published <- "published-online:"
  # } else if (length(grep("published-print:", ref_md)) >0 ) {
  #   published <- "published-print:"
  # }
  # published_date <- ref_md[grep(published, ref_md):length(ref_md)]
  # published_date <- gsub("  ", " ",  published_date)
  # published_date <- published_date[1:(grep("^ ", published_date, invert = TRUE)[2]-1)]
  # published_date <- paste0(gsub("[^0-9\\.]", "", published_date), collapse = "-")
  # published_date <- gsub("^-+", "", published_date)
  # # if (is.na(as.Date(as.character(published_date),format="%Y-%m-%d"))) published_date <- as.character(anydate(published_date))
  # if (is.na(as.Date(as.character(anydate(published_date))))) {
  #   published_date <- as.Date(as.character(published_date),format="%Y-%m-%d")
  # } else {
  #   published_date <- as.character(anydate(published_date))
  # }
  ref_date <- str_extract(grep("year =", ref_md, value = TRUE), "\\{(.*)\\}")
  ref_date <- gsub("\\{|\\}", "", ref_date)
  ref_date <- paste0("date: ", ref_date)


  # the title gets spread over several lines
  ref_title <- str_extract(grep("title =", ref_md, value = TRUE), "\\{(.*)\\}")
  ref_title <- gsub("\\{|\\}", "", ref_title)
  ref_title <- paste0("title: ", ref_title)
  
  ref_layout <- "paper"

  ref_journal <- str_extract(grep("journal =", ref_md, value = TRUE), "\\{(.*)\\}")
  ref_journal <- gsub("\\{|\\}", "", ref_journal)
  ref_journal <- paste0("journal: ", ref_journal)
  
  ## make article journal citation field (e.g. "<em>Cytometry Part A</em>, 81A('1'):25-34")
  # ref_cit <- paste(paste0("<em>", strsplit(ref_md[grep("^container-title:", ref_md)], ": ")[[1]][2], "</em>, "),
  #                  if (length(grep("volume:", ref_md)) > 0) strsplit(ref_md[grep("volume:", ref_md)], ": ")[[1]][2],
  #                  if (length(grep("issue:", ref_md)) > 0) paste0("(", strsplit(ref_md[grep("issue:", ref_md)], ": ")[[1]][2], ")"),
  #                  if (length(grep("page:", ref_md)) > 0) paste0(":", strsplit(ref_md[grep("page:", ref_md)], ": ")[[1]][2]),
  #                  sep = "")
  # ref_cit <- gsub("'", "", ref_cit)
  ref_cit <- paste0("ref: ", ref_journal)

    
  ref_abstract <- cr_abstract(doi = dois[ii])
  ref_abstract <- gsub("Abstract", "", ref_abstract, fixed=TRUE)
  
  ## make hugo-finite theme compliant *.md files from reference:
  ref_hugo_md <- c(ref_layout, ref_md, authors, ref_date, ref_cit, ref_journal, ref_vol, doi)
  if (length(grep("bstract:", ref_md)) == 0) {
    ref_abstract <- gsub("^title: ", "abstract: ", ref_title)
    ref_hugo_md <- c(ref_abstract, ref_hugo_md)
  }
  ref_hugo_md <- c("---", ref_hugo_md, "---", "# Abstract", ref_abstract )

 

  ## save the resulting ref_hugo_md into *.md file
  ref_name <- gsub("/", "_", gsub("'", "", gsub(" ", "_", paste(gsub("^title: ", "", grep("^title: ", ref_hugo_md, value = TRUE)[1])))))

  if (!file.exists(paste0("../papers/_posts/", ref_name, ".md"))) {
    write.table(ref_hugo_md, file = paste0(ref_name, ".md"), #"./content/publications/",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    print(paste(ii, ":", dois[ii], "---", ref_name))
  }

}
