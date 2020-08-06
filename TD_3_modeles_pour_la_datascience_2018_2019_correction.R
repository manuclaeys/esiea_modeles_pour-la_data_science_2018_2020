


####TD

DataURL <- read.csv("file:///C:/Users/claey/Documents/Cours/cour ESIEA/5A 2017-2018 DTE/Modèle pour la data science/TD3/urls.csv", sep=",")
df <- as.data.frame(DataURL)

###clean

dfUrl <- df[!(is.na(df$Text) | df$Text==""), ]
dfUrl <- dfUrl[! dfUrl$Text=="0", ]


# prepare corpus
##############
library(tm)
library(ggplot2)
library(lsa)
library(scatterplot3d)
library(SnowballC)


docs <- Corpus(VectorSource(dfUrl$Text[1:nrow(dfUrl)]))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("french"))  
docs <- tm_map(docs, stripWhitespace)  
#docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)  
tdm <- TermDocumentMatrix(docs)   










###########Exercice les topic
#load topic models library
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 2000
iter <- 500
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5
k <- 10

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#Avantage Gibbs
#http://web.univ-ubs.fr/lmam/gouno/BAYES/COURS/Cours6.pdf


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics

#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities





########Synonyme

findAssocs(dtm, "françois", corlimit=0.7) # specifying a correlation limit of 0.95   
findAssocs(dtm, "françois", corlimit=0.5) # specifying a correlation limit of 0.95   
findAssocs(dtm, "politique", corlimit=0.5) # specifying a correlation limit of 0.95 


# termes en colonne
matrice_docs_termes <- (dtm)
# find frequent words, works with either
findFreqTerms(matrice_docs_termes, 50)

#Reduce Matrix to terms used in most/all documents
inspect(removeSparseTerms(matrice_docs_termes, 0.4))
inspect(matrice_docs_termes)



#Test Zipf and Heaps laws
Zipf_plot(matrice_docs_termes)
Heaps_plot(matrice_docs_termes)


#Loi de Zipf qui consiste à comprendre comment la fréquence des mots
#est reliée à leur ordre. Ainsi, vous devrez calculer pour chaque mot sa fréquence et ordonner la liste
#de ces mots de manière décroissante et ainsi attribuer un rang à chaque mot. 

#comment la taille du vocabulaire
#d'une langue évolue par rapport à la taille des textes. Cette loi est connue sous le nom de Heap.


#######################Cluster###################"
mtd4.TfIdf <- (DocumentTermMatrix(docs, control=list(weighting=weightTfIdf)))
dim(mtd4.TfIdf)

dist4 <- dist(t(topicProbabilities), method="euclidean")
dist4
hc4 <- hclust(dist4, method="ward.D2")
plot(hc4)




library(wordcloud)
# Wordclouds
wordcloud(docs,
          scale=c(5,0.1), rot.per=0.35, 
          min.freq=5, max.words=50, use.r.layout=FALSE,
          colors= brewer.pal(8,"Spectral")
)
docs <- tm_map(docs, removeWords, c("plus", "dune"))

wordcloud(docs,
          scale=c(5,0.1), rot.per=0.35, 
          min.freq=5, max.words=50, use.r.layout=FALSE,
          colors= brewer.pal(8,"Spectral")
)





