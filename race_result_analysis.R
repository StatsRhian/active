
if(file.exists("C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/Arnside knott/AK2013seniorresults.pdf")){
  file_name <- "C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/Arnside knott/AK2013seniorresults.pdf" #new laptop
}else{
  file_name <- "E:/Dropbox/Mine/Personal/Running/Races&events/Arnside knott/AK2013seniorresults.pdf" #old laptop
}

#From pdf
install.packages("pdftools")
library(pdftools)

txt <- pdf_text(file_name)
txt
str(txt)
lines <- strsplit(txt,"\n")
head(lines[[1]][1])
lines[[1]][1]
head(lines[[1]])
cat(txt) #prints as a table by usingthe carriage returns /r in the text


#some experimental processing
lines[[1]] <- lines[[1]][-1]
header <- lines[[1]][1]
lines[[1]] <- lines[[1]][-1]
for(i in 1 : 4){
  cat(dim(lines[[i]]), "/r")
}

examp <- lines[[2]][1]
length(examp)

str(lines[[2]][1])
print(lines[[2]])
read.table(text = lines[[1]], row.names = NULL, header=F) #doesn't work
tab1 <- read.csv(text=unlist(lines))

cat(txt) #prints as a table by usingthe carriage returns /r in the text
tab2 <- read_lines(txt)
tab2
tab3 <- read_delim(txt, "/r")
tab3 <- read_delim(lines[[2]], " ")
tab4 <- read_tsv(lines[[2]])
textConnection(lines[[2]])

#From csv
library(readr)

res <- read_csv("Arnside knott/Arnside_Knott_2013_results.csv")
head(res)
res
