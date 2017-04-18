library('stringdist')
library('ggplot2')
library('tidyr')
options(stringsAsFactors = FALSE)

matched_data <- read.csv("upc matched test data.csv")

# Assumptions
#   All products must have a match
#   Matched names cannot be recycled
#   

product.names <- data.frame(id=matched_data$X_ID,
                            name=matched_data$amazonName,
                            matched_id=NA,
                            matched_name=NA,
                            match_distance=NA,
                            correct_match_name=matched_data$supplierName,
                            match_correct=NA)

match.candidates <- data.frame(id=matched_data$X_ID,
                               name=matched_data$supplierName,
                               matched_already=FALSE
                               )

cleanProducts <- function(set){
  set[which(set$name==""),"name"] <- NA
  set <- set[!is.na(set$name),]
  
  return(set)
}

product.names <- cleanProducts(product.names)
match.candidates <- cleanProducts(match.candidates)
                            
stringDistanceFormula <- function(string1, string2, method){
  string.distance <- stringdist(string1, string2, method=method)
  return(string.distance)
}

normalizeStringDistance <- function(string1, string2, distance, method){
  # (largerString.Length - editDistance) / largerString.Length)
  if (method == "cosine"){
    normalized.distance <- distance
  }
  else {
    larger.length <- max(nchar(string1), nchar(string2))
    normalized.distance <- distance/larger.length
  }
  return(normalized.distance)
}

applyStringDistance <- function(string1, string2, method){
  raw.distance <- stringDistanceFormula(string1, string2, method=method)
  string.distance <- normalizeStringDistance(string1, string2, raw.distance, method=method)
  return(string.distance)
}

# given amazon info & list of remaining supplier products
# find the most likely match
findMatch <- function(name, match.set, recycle.names = FALSE, match.must.exist=TRUE, max.norm.distance=NULL){
  # name: string, product name to be matched
  # match.set: data.frame, list of potential matches (id, name, matched)
  # recycle.names: boolean, whether to re-use already matched names
  # match.must.exist: boolean, whether match for name must exist in match.set
  # max.norm.distance: float: if match.must.exist==FALSE, the maximum distance that will return a match (must be between 0 and 1)
  available.matches <- match.set
  if (!recycle.names){
    available.matches <- available.matches[!available.matches$matched_already,]
  }
  available.matches$distance <- c(apply(available.matches[c("name")], 2, applyStringDistance, string1=name, method="cosine"))
  best.match <- available.matches[which(available.matches$distance==min(available.matches$distance)),]
  # need to account for tie for best match, currently assuming there is only 1 best match
  # if there is a tie, break it through random choice
  if(nrow(best.match) > 1){
    best.match <- best.match[round(runif(1, min=1, max=nrow(best.match)),0),]
  }
  
  if (!match.must.exist && best.match$distance > max.norm.distance){
    # some logic for denying the match if it is too far away
    # currently assuming a match must be made, so this is just placeholder
    best.match <- NULL
  }
  
  return(best.match)
}

associateMatch <- function(name.id, best.match, products){
  # name.id:
  # best.match:
  # products
  
  
  products[products$id==name.id,c("matched_id")] <- best.match$id
  products[products$id==name.id,c("matched_name")] <- best.match$name
  products[products$id==name.id,c("match_distance")] <- best.match$distance
  
  return(products)
}

matchAll <- function(product.names, match.candidates){
  for (i in 1:nrow(product.names)){
    product.name <- product.names[i,]
    # print(i)
    best.match <- findMatch(name=product.name$name, match.set=match.candidates, recycle.names = FALSE, match.must.exist=TRUE, max.norm.distance=NULL)
    if (!is.null(best.match)){
      product.names <- associateMatch(name.id=product.name$id, best.match=best.match, products=product.names)
      match.candidates[match.candidates$id==best.match$id,c("matched_already")] <- TRUE
    }
  }
  return(product.names)
}

result <- matchAll(product.names = product.names, match.candidates = match.candidates)

result[,"match_correct"] <- result[,"matched_name"]==result[,"correct_match_name"]


# Want to track:
  #   Matched Correctly
matched.correctly <- sum(result$match_correct)/nrow(result)
print(paste0("Correctly Matched: ",round(matched.correctly * 100,0),"%"))
  #   Matched Incorrectly
matched.incorrectly <- sum(!result$match_correct)/nrow(result)
print(paste0("Incorrectly Matched: ",round(matched.incorrectly * 100,0),"%"))
# ignoring unmatched for now since we are testing only with a must-match strategy
# instead we test that the sums are correct
print(paste0("Matched %'s Sum to 1: ", (matched.correctly + matched.incorrectly)==1))
  #   Unmatched Correctly
  #   Unmatched Incorrectly

# Begin to explore match_distance limiting by plotting logistic regression
# i.e. how well does match_distance predict a correct match
model <- glm(formula = match_correct ~ match_distance, family = binomial, data = result)
print(summary(model))

intercept <- model$coef[1] # intercept
distance_coef <- model$coef[2]

distance_range <- seq(from=min(result$match_distance), to=max(result$match_distance), by=.01)

logits <- intercept + distance_coef * distance_range

probs <- exp(logits)/(1 + exp(logits))

plot(distance_range, probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="turquoise2", 
     xlab="distance", ylab="P(correct match)", main="Probability of closest distance product being a correct match")
abline(h=.5, lty=2)
points(result$match_distance, result$match_correct)

# In theory we could then feed these results back into inform max.norm.distance in the matchAll function for future match attempts
# Given more data, we can test how well a more informed prediction performs
# Also, given other information such as barcodes and use of the product URL may result in a better outcome