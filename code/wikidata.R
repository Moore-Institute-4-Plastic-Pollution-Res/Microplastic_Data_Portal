#install.packages("WikidataR")
library(WikidataR)

wikidata_api <- read.table("secrets/wikidata.txt")

item <- find_item("Paracetamol")
property <- find_property("medical condition treated")
qid <- qid_from_identifier('ISBN-13','978-0-262-53817-6')
identifier <- identifier_from_identifier('ORCID iD','IMDb ID',c('0000-0002-7865-7235','0000-0003-1079-5604'))
article.qid      <- qid_from_DOI(c('10.15347/WJM/2017.007','10.15347/WJM/2019.001','10.15347/WJM/2019.007'))
article.q        <- get_item(article.qid)
article.topics.p <- extract_claims(article.q, "main topic")
get_names_from_properties(article.topics.p)

query_wikidata('SELECT DISTINCT
  ?genre ?genreLabel
WHERE {
  ?film wdt:P31 wd:Q11424.
  ?film rdfs:label "The Cabin in the Woods"@en.
  ?film wdt:P136 ?genre.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}')

write_wikidata(items      = c("Q4115189","Q13406268"),
               properties = "author",
               values     = c("Q762","Q41406"),
               format     = "api",
               api.username = "wincowgerDEV", # Enter your Wikimedia username here
               api.token  = wikidata_api$V1 #REDACTED# Find yours from https://tools.wmflabs.org/quickstatements/#/user
)

sparql_query <- 'SELECT ?Article ?ArticleLabel ?JLabel ?T ?peer_review_URL WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
  ?Article wdt:P1433 wd:Q24657325.
  OPTIONAL { ?Article wdt:P1433 ?J. }
  OPTIONAL { ?Article wdt:P1476 ?T. }
  OPTIONAL { ?Article wdt:P7347 ?peer_review_URL. }}
LIMIT 10000'
articles.qr <- as_tibble(query_wikidata(sparql_query))
articles.qr <- articles.qr[articles.qr$peer_review_URL=="",] #omit those with review URLs listed
review.URLs <- paste0('https://en.wikiversity.org/wiki/Talk:',
                      articles.qr$JLabel,
                      "/",
                      articles.qr$T
)
review.URLs <- gsub(" ","_",review.URLs)

write_wikidata(items      = sapply(sapply(articles.qr$Article,pattern = "/",stringr::str_split),tail,1),
               properties = "Peer review URL",
               values     = review.URLs,
               format     = "tibble",
)

write_wikidata(items        = sapply(sapply(articles.qr$Article,pattern = "/",stringr::str_split),tail,1),
               properties   = "Peer review URL",
               values       = review.URLs,
               format       = "api",
               api.username = "myusername", 
               api.token    = , #REDACTED# Find yours from https://tools.wmflabs.org/quickstatements/#/user
)
