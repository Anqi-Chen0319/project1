library("dplyr")
library("xml2")
library("rvest")
library("stringr")

# Download files
url1 <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2019/master/data/scholar/abhijit_banerjee_GoogleScholarCitations.html"
url2 <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2019/master/data/scholar/esther_duflo_GoogleScholarCitations.html"

download.file(url1,  "./data/rawdata/scholarA_GoogleScholarCitations.html")
download.file(url2,  "./data/rawdata/scholarB_GoogleScholarCitations.html")

# # 1) Extract simple information of the authors.
## Extract the names of the scholars from the HTML object
scholara <- read_html("./data/rawdata/scholarA_GoogleScholarCitations.html")
scholarb <- read_html("./data/rawdata/scholarB_GoogleScholarCitations.html")
tita <- scholara %>% html_nodes(css = ".gs_gray") %>% html_text()
scholarA <- tita[seq(1,990,by = 2)]
data.frame(scholarA)
titb <- scholarb %>% html_nodes(css = ".gs_gray") %>% html_text()
scholarB <- titb[seq(1,990,by = 2)]
dat.frame(scholarB)

## Extract the scholars' affiliated institutions from the HTML object (NA if not specified)
scholara %>% html_nodes(css = ".gsc_prf_ila") %>% html_text()
scholarb %>% html_nodes(css = ".gsc_prf_ila") %>% html_text()

# Extract all the papers for each author (not just the 20 most cited)
## For scholar A
parent_data <- read_html("./data/rawdata/scholarA_GoogleScholarCitations.html")
tablea <- html_table(parent_data)
info_nodesa <- parent_data %>% 
  html_nodes(xpath='//*[@id="gsc_a_b"]') %>%
  html_nodes(xpath="tr") %>%
  html_nodes(xpath="td")
children_nodesa = html_children(info_nodesa)
table_texta = sapply(children_nodesa, html_text)
refined_table_texta = table_texta[table_texta != "*"]
titlea = refined_table_texta[seq(from = 1, to = length(refined_table_texta), by =5)]
authora = refined_table_texta[seq(from = 2, to = length(refined_table_texta), by =5)]
journala = refined_table_texta[seq(from = 3, to = length(refined_table_texta), by =5)]
citationa = refined_table_texta[seq(from =4, to = length(refined_table_texta), by = 5)]
yearsa = refined_table_texta[seq(from =5, to = length(refined_table_texta), by = 5)]
dfa = data.frame(titlea,authora,journala,citationa,yearsa)
dfa
write.csv(dfa,"./data/cleandata/scholarA.csv")

## fOR Scholar B
parent_datab <- read_html("./data/rawdata/scholarB_GoogleScholarCitations.html")
tableb <- html_table(parent_datab)
info_nodesb <- parent_datab %>% 
  html_nodes(xpath='//*[@id="gsc_a_b"]') %>%
  html_nodes(xpath="tr") %>%
  html_nodes(xpath="td")
children_nodesb = html_children(info_nodesb)
table_textb = sapply(children_nodesb, html_text)
refined_table_textb = table_textb[table_textb != "*"]
titleb = refined_table_textb[seq(from = 1, to = length(refined_table_textb), by =5)]
authorb = refined_table_textb[seq(from = 2, to = length(refined_table_textb), by =5)]
journalb = refined_table_textb[seq(from = 3, to = length(refined_table_textb), by =5)]
citationb = refined_table_textb[seq(from =4, to = length(refined_table_textb), by = 5)]
yearsb = refined_table_textb[seq(from =5, to = length(refined_table_textb), by = 5)]
dfb = data.frame(titleb,authorb,journalb,citationb,yearsb)
dfb
write.csv(dfb,"./data/cleandata/scholaraB.csv")

