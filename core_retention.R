library(NLP)
library(tm)
library(openNLP)
library(graph)
library(igraph)
library(sna)


ConstructTextGraph <- function(n) { 
  word_graph <- new("graphNEL")
  i <- 1
  while (i < length(words) ) {
    if ( IsSelectedWord(words[i]) ) {                                   
      links <- GetWordLinks(i,n)       
      if (links[1] != "") {                                     
        cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
        if ( length(which(nodes(word_graph)==words[i]))==0  ) {     
          word_graph <- addNode(words[i],word_graph)
        }                                               
        
        for (j in 1:length(links)) {
          if ( length(which(graph::nodes(word_graph)==links[j]))==0 ) {
            word_graph <- addNode(links[j],word_graph)
            word_graph <- addEdge(words[i],links[j],word_graph,1)
          } 
          else {
            if ( (length(which(graph::edges(word_graph,links[j])[[1]]==words[i]))>0 ) ) { 
              prev_edge_weight <- as.numeric(edgeData(word_graph,words[i],links[j],"weight"))
              edgeData(word_graph,words[i],links[j],"weight") <- prev_edge_weight+1
            }
            else {
              word_graph <- addEdge(words[i],links[j],word_graph,1)
            }
          } 
        }
      }
    }
    i <- i+1
  }
  word_graph
}
#Remove Tags from Pos Tagged Words
RemoveTags <- function(Words) {
  sub("/[A-Z]{2,3}","",Words)
}
#To select Final Tagged words from Word List
SelectTaggedWords <- function(Words,tagID) {
  Words[ grep(tagID,Words) ]
}
#Apply PosTags to All tokens 
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
 POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

#To check if word is present in word list
IsSelectedWord <- function(Word) {
  ifelse(length(which(selected_words == Word))>0, TRUE, FALSE)
}

#to get all words in its window size
GetWordLinks <- function(position,scope) {
  scope <- ifelse(position+scope>length(words),length(words),position+scope)
  links <- ""
  for (i in (position+1):scope) { 
    if ( IsSelectedWord(words[i]) ) links <- c(links,words[i])
  }
  
  if (length(links)>1) {
    links[2:length(links)]
  }
  else {
    links <- ""
  }
}

#split corpus text in words
SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase," "))
}

#text= read.table(file.choose())
text<-c("A method for solution of systems of linear algebraic equations with m-dimensional lambda -matrices.
        A system of linear algebraic equations with m-dimensional lambda -matrices is considered.
        The propose method of searching for the solution of this system lies in reducing it to a numerical system of a special kind.")
corp <- Corpus(VectorSource(text))
corp <- tm_map(corp,content_transformer(tolower))
corp <- tm_map(corp, removePunctuation)
#corp <- tm_map(corp, removeNumbers)
tagged_text <- tagPOS(corp[[1]])

corp<-tm_map(corp,removeWords,stopwords(kind = "english"))
corp<- tm_map(corp,stemDocument)
words<-SplitText(as.character(corp[[1]]))
words

tagged_words <- SplitText(as.character(tagged_text))
tagged_words
tagged_words <- c(SelectTaggedWords(tagged_words,"/NN"),SelectTaggedWords(tagged_words,"/JJ"))
tagged_words<-RemoveTags(tagged_words)
tagged_words<-removeWords(tagged_words, stopwords("english"))
tagged_words<- stemDocument(tagged_words,language = "english")

selected_words<- unique(tagged_words)
selected_words
text_graph <- ConstructTextGraph(4)


plot(text_graph, attrs = list(node = list(fillcolor = "lightblue", fontsize = 60),edge = list(arrowsize=0.5)))
x<-igraph.from.graphNEL(text_graph, name = TRUE, weight = TRUE,unlist.attrs = TRUE)
cbind( get.edgelist(x) , round( E(x)$weight, 3 ))

x<-simplify(x,remove.loops=TRUE)

coreness <- graph.coreness(x)
coreness
maxCoreness <- max(coreness)
verticesHavingMaxCoreness <- which(coreness == maxCoreness) 

kcore <- induced.subgraph(graph=x,vids=verticesHavingMaxCoreness)
#kcore<-simplify(kcore,remove.loops=TRUE)

tkplot(kcore,vertex.label=get.vertex.attribute(kcore,name='vert.names',index=V(kcore)),edge.color="black",vertex.color="white",vertex.size=20,edge.label=get.edge.attribute(kcore,"weight",index = E(kcore)))

