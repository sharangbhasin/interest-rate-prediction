##understanding the reason for loan provided using text analysis
library(tm)
library(SnowballC)
head(data[,c("X17","X10")])
head(data[,c("X16")])

## Loanee Title NLP

corpus = Corpus(VectorSource(data$X17))
corpus = tm_map(corpus, tolower) # lower case
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords('english')) # from library
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, PlainTextDocument)
dtm_title = DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_title = removeSparseTerms(dtm_title, 0.99)
dtm_title = as.data.frame(as.matrix(dtm_title))
rownames(dtm_title) = NULL
names(dtm_title) = paste(names(dtm_title),sep=" ","title")
names(dtm_title)


## Loanee Employee Title NLP

corpus = Corpus(VectorSource(data$X10))
corpus = tm_map(corpus, tolower) # lower case
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords('english')) # from library
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, PlainTextDocument)
dtm_emp_title = DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_emp_title = removeSparseTerms(dtm_emp_title, 0.99)
dtm_emp_title = as.data.frame(as.matrix(dtm_emp_title))
rownames(dtm_emp_title) = NULL
names(dtm_emp_title) = paste(names(dtm_emp_title),sep=" ","emp_title")
names(dtm_emp_title)

## Loanee desc NLP

corpus = Corpus(VectorSource(data$X16))
corpus = tm_map(corpus, tolower) # lower case
corpus = tm_map(corpus, removeNumbers)
corpua = tm_map(corpus, removeWords, stopwords('english')) # from library
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, PlainTextDocument)
dtm_desc = DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_desc = removeSparseTerms(dtm_desc, 0.99)
dtm_desc = as.data.frame(as.matrix(dtm_desc))
rownames(dtm_desc) = NULL
names(dtm_desc) = paste(names(dtm_desc),sep=" ","desc")
names(dtm_desc)


rm(corpus)


##sentimental analysis of X16
#load libraries
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(devtools)
install_github('sentiment140', 'okugami79')
library(sentiment)

##subset the data
subsetdata=subset(data,select=c("X16"))

#Delete the leading spaces
subsetdata$X16 = str_trim(subsetdata$X16)


##names(subsetdata) = "Reason for loan provided by borrower"
subsetdata$X16 = as.character(subsetdata$X16)

##Pre-processing
#convert comments into corpus
subsetdataCorpus = Corpus(VectorSource(subsetdata$X16))
writeLines(as.character(subsetdataCorpus[[2]]))##to see the document 2

#case folding
subsetdataCorpus = tm_map(subsetdataCorpus, tolower)

#remove stop words
subsetdataCorpus = tm_map(subsetdataCorpus, removeWords, stopwords('english'))

#remove punctuation marks
subsetdataCorpus = tm_map(subsetdataCorpus, removePunctuation)

#remove num bers
subsetdataCorpus = tm_map(subsetdataCorpus, removeNumbers)

#remove unnecesary spaces
subsetdataCorpus = tm_map(subsetdataCorpus, stripWhitespace)

#convert into plain text
subsetdataCorpus = tm_map(subsetdataCorpus, PlainTextDocument)

#create corpus
subsetdataCorpus = Corpus(VectorSource(subsetdataCorpus))

##remove stop words using the csv file
stop_words = read.csv("stopwords.csv", header = T)
names(stop_words) = "StopWords"
subsetdataCorpus_WC = subsetdataCorpus
subsetdataCorpus_WC = tm_map(subsetdataCorpus_WC, removeWords, stop_words$StopWords)
postCorpus_WC = tm_map(subsetdataCorpus_WC, removeWords, c('rid','etc','try','offer','process','left','period','cut','fix','look','fixed','little','extra',
                                                           'added','cards','auto','half','value','pool','people','major','considering','history','greatly','request','moving','add','past','ago','late',
                                                           'cut', 'goal','family','fix','track','single','yrs','hard',stopwords('english')))

# wordcloud
pal2 = brewer.pal(8,"Dark2")
png("wordcloud.png", width = 12, height = 8, units = 'in', res = 200)
wordcloud(subsetdataCorpus_WC, scale = c(5,.2), min.freq = 30, max.words = 300, random.order = FALSE, rot.per = .15, colors = pal2)
dev.off()

#Build document term matrix
tdm = TermDocumentMatrix(subsetdataCorpus)

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 100)
