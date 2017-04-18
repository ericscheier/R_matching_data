#install.packages("jsonlite")
library(jsonlite)

supplierData <- fromJSON(paste0(getwd(), "/SupplierProducts.json"))#fromJSON("C:\\Users\\mike\\Documents\\R\\SupplierProducts.json")
amazonData <- fromJSON(paste0(getwd(), "/AmazonProducts.json"))#fromJSON("C:\\Users\\mike\\Documents\\R\\amazonProducts.json")



#install.packages("stringdist")
library('stringdist')
#install.packages("dplyr")
library('dplyr')
supplierName <-  c(supplierData$supplierName)
amazonName <-  c(amazonData$amazonName)

sMatrix <- stringdistmatrix(supplierName,amazonName,useNames="strings",method="cosine")

sMatrixdf <- data.frame(as.matrix(sMatrix))

idx <- apply(sMatrix, 2, function(x) x >0 & x<0.035)
idx <- apply(idx, 1:2, function(x) if(isTRUE(x)) x<-1 else x<-NA)

#install.packages("reshape2")
library(reshape2)
final <- melt(idx) %>%
  filter(value==1) %>%
  select(Var1, Var2)

final[] <- lapply(final, as.character)

final <- final[!duplicated(data.frame(list(do.call(pmin,final),do.call(pmax,final)))),]

names(final) <- c('string 1', 'string 2')

# We know that a product cannot be matched more than once, 
# so we much iterate the function each time
# and remove products that have been matched


# Want to track:
#   Matched Correctly
#   Matched Incorrectly
#   Unmatched Correctly
#   Unmatched Incorrectly

# Let's plot the distances against the actual matches to see what threshold we should apply next time

# Table should have:
#   supplierID
#   amazonID
#   distance
#   match_confirmed

# Need to normalize to percent:
#   percent = (largerString.Length - editDistance) / largerString.Length)

# http://www.markvanderloo.eu/yaRb/2013/08/09/approximate-string-matching-in-r/
# https://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/