install.packages("rvest")
library("rvest")

arnside2013 <- read_html("http://www.fellrunner.org.uk/results.php?id=2195")

arnside2013[2]
