library("rcrossref")
library("yaml")
library("anytime")
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
  ref_md <- cr_cn(dois = dois[ii])#, format = "citeproc-json")
  ref_md <- as.yaml(ref_md)
  ref_md <- unlist(strsplit(ref_md, "\n"))

  ## change and construct yaml fields
  url_doi <- gsub("URL:", "url_doi:", grep("^URL: ", ref_md, value = TRUE))
  doi <- gsub("DOI:", "doi:", grep("^DOI: ", ref_md, value = TRUE))
  # authors from first occurance of "author:", the second are authors of cited work
  authors <- ref_md[(grep("author:", ref_md)[1]+1):(grep("  affiliation:", ref_md)[1]-1)]
  authors <- apply(matrix(authors, ncol = 2)[-1, ] , 1 , paste , collapse = " ")
  authors <- gsub("   - ", " ", authors)
  authors <- gsub("  -", "-", authors)
  authors <- c("authors:", authors)

  if (length(grep("published-online:", ref_md)) >0 ) {
    published <- "published-online:"
  } else if (length(grep("published-print:", ref_md)) >0 ) {
    published <- "published-print:"
  }
  published_date <- ref_md[grep(published, ref_md):length(ref_md)]
  published_date <- gsub("  ", " ",  published_date)
  published_date <- published_date[1:(grep("^ ", published_date, invert = TRUE)[2]-1)]
  published_date <- paste0(gsub("[^0-9\\.]", "", published_date), collapse = "-")
  published_date <- gsub("^-+", "", published_date)
  # if (is.na(as.Date(as.character(published_date),format="%Y-%m-%d"))) published_date <- as.character(anydate(published_date))
  if (is.na(as.Date(as.character(anydate(published_date))))) {
    published_date <- as.Date(as.character(published_date),format="%Y-%m-%d")
  } else {
    published_date <- as.character(anydate(published_date))
  }
  ref_date <- paste0("date: ", published_date)

  ## make article journal citation field (e.g. "<em>Cytometry Part A</em>, 81A('1'):25-34")
  ref_cit <- paste(paste0("<em>", strsplit(ref_md[grep("^container-title:", ref_md)], ": ")[[1]][2], "</em>, "),
                   if (length(grep("volume:", ref_md)) > 0) strsplit(ref_md[grep("volume:", ref_md)], ": ")[[1]][2],
                   if (length(grep("issue:", ref_md)) > 0) paste0("(", strsplit(ref_md[grep("issue:", ref_md)], ": ")[[1]][2], ")"),
                   if (length(grep("page:", ref_md)) > 0) paste0(":", strsplit(ref_md[grep("page:", ref_md)], ": ")[[1]][2]),
                   sep = "")
  ref_cit <- gsub("'", "", ref_cit)
  ref_cit <- paste0("citation: ", ref_cit)

  # the title gets spread over several lines
  ref_title <- ref_md[grep("^title: ", ref_md):length(ref_md)]
  ref_title <- gsub("  ", " ",  ref_title)
  ref_title <- paste0(ref_title[1:(grep("^ ", ref_title, invert = TRUE)[2]-1)], collapse = "")
  ref_md <- gsub("^title: ", "title-spread: ", ref_md) # changing the original title

  ## make hugo-finite theme compliant *.md files from reference:
  ref_hugo_md <- c(ref_title, url_doi, doi, authors, ref_date, ref_cit, ref_md)
  if (length(grep("bstract:", ref_md)) == 0) {
    ref_abstract <- gsub("^title: ", "abstract: ", ref_title)
    ref_hugo_md <- c(ref_abstract, ref_hugo_md)
  }
  ref_hugo_md <- c("---", ref_hugo_md, "---")

  # hugo-finite does not like type: and URL: in lines (just needs proper type??, relative urls??)
  ref_hugo_md <- ref_hugo_md[-grep("^URL: ", ref_hugo_md)]
  ref_hugo_md <- ref_hugo_md[-grep("^type: ", ref_hugo_md)]

  ## save the resulting ref_hugo_md into *.md file
  ref_name <- gsub("/", "_", gsub("'", "", gsub(" ", "_", paste(gsub("^title: ", "", grep("^title: ", ref_hugo_md, value = TRUE)[1])))))

  if (!file.exists(paste0("../papers/_posts/", ref_name, ".md"))) {
    write.table(ref_hugo_md, file = paste0(ref_name, ".md"), #"./content/publications/",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    print(paste(ii, ":", dois[ii], "---", ref_name))
  }

}
