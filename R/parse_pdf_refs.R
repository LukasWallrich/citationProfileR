
# library(httr)
# #giving 200 but content is returned in hexidecimal
# parse_pdf_refs <- function(file) {
#   #creating API url
#   GROBID_URL <- 'https://cloud.science-miner.com/grobid'
#   api <- '/api/processReferences'
#   api_url <- paste0(GROBID_URL, url)
#
#   #input is required, API call to GROBID process Reference(https://grobid.readthedocs.io/en/latest/Grobid-service/)
#   GROBID <- httr::POST(api_url, accept("application/x-bibtex"),  body = list(input = upload_file(file)), encode = "multipart", verbose())
#
#   # translate return from Raw to Characters
#   output <- rawToChar(GROBID$content)
#   return(output)
# }
#
# #file <- "/Users/adrianabeltran/Downloads/brewerPaper.pdf"
# file <- "/Users/adrianabeltran/Downloads/practicing-moderation-community-moderation-as-reflective-practice (1).pdf"
# output <- parse_pdf_refs(file)
#
# # translating this
# # def parse_pdf_refs(file):
# #   import requests
# # GROBID_URL = 'https://cloud.science-miner.com/grobid'
# # url = '%s/api/processReferences' % GROBID_URL
# # xml = requests.post(url, files={'input': open(file, 'rb')}, headers = {"Accept": "application/x-bibtex"}, data = {"consolidateCitations": "1"})
# # return xml.text
