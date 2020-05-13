# Here is the code Neal came up with for redirects

doi<-"https://doi.org/10.1016/j.envint.2015.07.011"
newurl <- readLines(curl::curl(as.character(httr::GET(doi)[1])))[15]
test<-gsub("%3A",":",gsub("%2F","/",newurl))
doi<-gsub("^.*direct=\\s*|\\s*%3Fvia.*$", "", test)


#Starting to make Neals code function/functions

# First one is Url to text - I have it printing the text out which is not very useful
#- I will just expand the function but had a spare 5 minutes to begin to play!

url_to_text<-function(url=url){
  library(dplyr)
  library(httr)
  library(rvest)
  library(htm2txt)

  get_object<-GET(url)
  cat(content(get_object, "text"), file="temp.html")


  #html_object<-read_html(x)
  #write_xml(html_object, file="temp.html")

  #extract text from URL (no field codes)

  text <- gettxt(url)
  print(text)



}

#test url_to_text

url<-"https://doi.org/10.1186/s13750-015-0044-5"

url_to_text(url = url)






#Add data folder and test.RDS file

test <- readRDS("data/test.RDS")

#Neals code
# setwd("~/OneDrive - SEI/ESHackathon/4.Remote 2020/doi2txt")
# dois<-read.csv(file.choose(),header=TRUE)
# attach(dois)
# names(dois)
# str(dois)
# str(url)



#save an HTML from a URL
#url<-dois
library(dplyr)
#url %>% mutate_if(is.factor, as.character) -> url
url<-"https://doi.org/10.1186/s13750-015-0044-5"
library(httr)
get_object<-GET(url)
cat(content(get_object, "text"), file="temp.html")

library(rvest)
html_object<-read_html(url)
write_xml(html_object, file="temp.html")

#extract text from URL (no field codes)
library(htm2txt)
text <- gettxt(url)

#split text by line breaks
lines <- strsplit(text, "\n")[[1]]
if(any(lines %in% c(" ", ""))){
  lines <- lines[-which(lines %in% c(" ", ""))]
}

#extract table legends
tablelegendtext<-(grep("Full size table",lines)-2)
tablelegends<-lines[tablelegendtext]

#extract tables - split the html code by lines, extract lines referring to full size tables, extract links to tables, download table htmls, parse tables based on selection of table html file (manual)
lines2<-strsplit(as.character(html_object), "\n")[[1]]
head(lines2)
if(any(lines2 %in% c(" ", ""))){
  lines2<-lines2[-which(lines2 %in% c(" ", ""))]
}
tables<-grep("Full size table",lines2,value=TRUE);rm(lines2)
library(stringr)
tables<-str_match(tables,"/articles(.*?)><span>Full size")
tables<-sub('\"',"",sub(" ","",paste("https://environmentalevidencejournal.biomedcentral.com/articles",tables[,2])))
for(i in 1:length(tables)){
  html_object1<-read_html(tables[i])
  somefilename<-paste0("filename_", i, ".html")
  write_xml(html_object1,file=somefilename)
}
library(rvest)
turl<-tables[1]
temp_table<-readHTMLTable(file.choose())


#extract figures - split the html code by lines, extract lines referring to full size figures, extract links to figures, download figure htmls,save images as files
lines2<-strsplit(as.character(html_object), "\n")[[1]]
head(lines2)
if(any(lines2 %in% c(" ", ""))){
  lines2<-lines2[-which(lines2 %in% c(" ", ""))]
}
figures<-grep("Full size image",lines2,value=TRUE);rm(lines2)
library(stringr)
figures<-str_match(figures,"/articles(.*?)data-track")
figures<-substr(sub(" ","",sub(" ","",sub('\" data-track',"",paste("https://environmentalevidencejournal.biomedcentral.com/articles",figures[,2])))),1,nchar(sub(" ","",sub(" ","",sub('\" data-track',"",paste("https://environmentalevidencejournal.biomedcentral.com/articles",figures[,2])))))-1)
for(i in 1:length(figures)){
  html_object2<-read_html(figures[i])
  somefilename<-paste0("filename_", i, ".html")
  write_xml(html_object2,file=somefilename)
}
html_object_fig<-read_html("filename_1.html")
lines3<-strsplit(as.character(html_object_fig), "\n")[[1]]
if(any(lines3 %in% c(" ", ""))){
  lines3<-lines3[-which(lines3 %in% c(" ", ""))]
}
figurelinks<-grep("image/art",lines3,value=TRUE)
library(stringr)
figurelinks<-str_match(figurelinks,"media(.*?)webp")[1]
figurelinks<-sub("webp","png",sub(" ","",paste("https://",figurelinks)))
download.file(figurelinks,destfile="figure1.png")



#detect start of the methods
methods <- grep("methods", tolower(lines))
methods <- methods[which.min(nchar(lines[methods]))]
lines[methods]
#detect the start of the results
results <- grep("results", tolower(lines))
results <- results[which.min(nchar(lines[results]))]

methods_section <- lines[methods:(results-1)]


#extract values from text
abstract<-grep("Abstract",lines)

doi<-sub("DOI: ","",grep("DOI:",lines,value=TRUE))

citationtext<-grep("Cite this article",lines)+1
citation<-lines[citationtext][2]

received<-sub("Received: ","",grep("Received",lines,value=TRUE))

accepted<-sub("Accepted: ","",grep("Accepted",lines,value=TRUE))

publishedtext<-grep("Published",lines,value=TRUE)[2]
published<-sub("Published: ","",publishedtext)

coitext<-grep("Competing interests",lines)
coi<-sub("Competing interests ","",lines[coitext][1])

supporttext<-grep("Sources of support",lines)
support<-sub("Sources of support ","",lines[supporttext][1])

acknowtext<-grep("Acknowledgements",lines)+1
acknowledgements<-lines[acknowtext]

accesses<-sub("Accesses","",grep("Accesses",lines,value=TRUE))

citations<-sub("Citations","",grep("Citations",lines,value=TRUE))

altmetric<-sub("Altmetric","",grep("Altmetric",lines,value=TRUE))

#delimiting and extracting background text - based on occurrence of the word, so possible bug source
backgroundtext<-(grep("Background",lines)[2])+1
objectivetext<-((grep("Objective",lines)[1])-1)
background<-lines[backgroundtext:objectivetext]

#delimiting and extracting objectives text - based on occurrence of the word, so possible bug source
objectivetext<-((grep("Objective",lines)[1])+1)
methodstext<-((grep("Methods",lines)[2])-1)
objective<-lines[objectivetext:methodstext]

#delimiting and extracting methods text - based on occurrence of the word, so possible bug source
methodstext<-(grep("Methods",lines)[2])+1
resultstext<-(grep("Results",lines)[2])-1
methods<-lines[methodstext:resultstext]

#inclusion criteria - mix of those coming from objectives and methods sections
population<-sub("Population: ","",grep("population:",lines,value=TRUE,ignore.case=TRUE))
intervention<-sub("Intervention: ","",grep("intervention:",lines,value=TRUE,ignore.case=TRUE))
exposure<-sub("Exposure: ","",grep("exposure:",lines,value=TRUE,ignore.case=TRUE))
comparator<-sub("Comparator: ","",grep("comparator:",lines,value=TRUE,ignore.case=TRUE))
outcome<-sub("Outcome: ","",grep("outcome:",lines,value=TRUE,ignore.case=TRUE))

#delimiting and extracting results text - based on occurrence of the word, so possible bug source
resultstext<-(grep("Results",lines)[2])+1
discussiontext<-(grep("Discussion",lines)-1)
results<-lines[methodstext:discussiontext]

#delimiting and extracting discussion and conclusions text - based on occurrence of the word, so possible bug source
discussiontext<-(grep("Discussion",lines)+1)
referencetext<-(grep("References",lines)-1)
discussion<-lines[discussiontext:referencestext]

#delimiting and extracting references text - based on occurrence of the word, so possible bug source
referencetext<-(grep("References",lines)+1)
contributions<-(grep("Authors’ contributions",lines)-1)
references<-lines[referencetext:contributions]
references<-references[-grep("Google Scholar",data.frame(references)[,1])] #remove rows containing "Google Scholar"
references<-references[-grep("CAS",data.frame(references)[,1])] #remove rows containing "CAS"
references<-references[-grep("Download references",data.frame(references)[,1])] #remove rows containing "Download references"
references<-references[-grep("•",data.frame(references)[,1])] #remove rows containing "•"


#save PDF - extract doi url ending, paste to generate pDF link, then downlaod
doinum<-sub("https://doi.org/","",doi)
pdfurl<-sub(" ","",paste("https://environmentalevidencejournal.biomedcentral.com/track/pdf/",doinum))
destfile<-sub("/","_",doinum)
download.file(pdfurl,paste(destfile,".pdf"))

