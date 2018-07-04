install.packages("tm")  # pour le text mining
install.packages("SnowballC") # pour le text stemming
install.packages("wordcloud") # générateur de word-cloud 
install.packages("RColorBrewer") # Palettes de couleurs
# Charger les packages 
#selon le site http://data-bzh.fr/text-mining-r-part-1/
install.packages("tidytext")
install.packages("proustr")
install.packages("tidyverse")
install.packages("stopwords")
library("tm")
library("tidytext")
library("proustr")
library("tidyverse")
#?? devtools::install_github("ThinkRstat/stopwords") 
###  chargements des mots de liaisons inutiles ###
library("stopwords") 
stopwords_iso_fr <- stopwords(language = "fr") #classe caracteres
# Méthode 1.1  : Tidytext
#Nous commençons par utiliser la méthode du tidytext, décrite dans l'ouvrage "Tidy Text-mining with R".
# colnames(books_tidy)
#[1] "word" "n"   
# class(books_tidy)
# "tbl_df"     "tbl"        "data.frame"

books_tidy <- proust_books() %>% 
	mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
	unnest_tokens(word, text) %>%
    filter(!word %in% stopwords('fr')) %>%
	count(word, sort = TRUE)
	
########################################################################################################"""
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringr")
library("stringdist")
#Chargement du texte : Corpus() du package tm. 
# Lire le fichier texte
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
texte <- readLines("montexte.txt") #Adresse relative. 
texte <- tolower(texte)
texte.words <- unlist(strsplit(texte,"[[:blank:].]+")) #décomposition du texte en mots 
texte.words <- na.omit(str_match(texte.words,"[^0-9]+")) #Nettoyage du texte des chiffres mais renvoie un type matrix
texte.words <- as.character(na.omit(str_match(texte,"[a-zA-zéèçà]{4,18}"))) #Enlèvement de mots de liaison non significatifs

print(str_c("la classe de l'objet texte.words est ",class(texte.words)))
texte.words <- sort(unique(as.character(texte.words))) # force la classe caractère 
print("application de la fonction as.character()")
print(str_c("la classe de l'objet texte.words est ",class(texte.words)))
#Calcul de la matrice de distance des mots de texte.words. les distances, définies par la méthode de seq_distmatrix 
#sont : cosine,hamming,qgram,jaccard,jw. 
#Ce calcul de la matrice de distance (package stringDist et fonction seq_distmatrix()) nécéssite de transformer 
#les mots du texte en un codage d'entier (une forme de hashage) via lapply(vecteurDeMots,utf8ToInt)
#Ce codage permet de rendre les algorithmes de calculs de distances généralisable et plus performants que de manipuler des caractères. 
hashcode.words <- lapply(texte.words,utf8ToInt)
tabDist <- seq_distmatrix(hashcode.words,hashcode.words,method="jaccard")
tabDist <- as.dist(tabDist)
#Cette matrice de distance peut alors utiliser l'analyse en coordonnées principales pour une rprésentation planaire (library ade4) 
#Attention ! si la matrice n'est pas euclidienne, ce qui est le cas , aucune représentation n'est possible. utiliser la fonction is.euclid()
#cf ci-dessous 
#Visualisation d'une classification arborescence hiérarchique
res.clust <- hclust(tabDist)
plot(res.clust,labels=texte.words,cex=1)
distTableConv2 <- function(d) # d est une table de distance obtenue avec dist()
{
#gdist <- data.frame(x=runif(n,min=-10,max=10),y=runif(n,min=-10,max=10),row.names = LETTERS[1:n])
library(ade4) 
#data(capitales)
#d <- dist(capitales$xy)
dd <- -0.5*bicenter.wt(d*d)
dde <- eigen(dd)
ds <- pcoscaled(d)
plot(ds,asp=1)
text(ds[,1],ds[,2],labels=letters[1:nrow(as.matrix(d))],lwd=5,cex=2)
return(list(ds,dde))
}
distTableConv2(as.dist(tabDist))
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
# Charger les données comme un corpus. Attention aux paramétrages , UTF-8. 
docs <- Corpus(VectorSource(texte)) 
#Le contenu du corpus se consulte par inspect(docs)
inspect(docs)
#Nettoyage du texte de premier niveau 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#Nettoyage du texte de second niveau 
# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("english"))
# Supprimer votre propre liste de mots non désirés
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#---------------------------------------------------#
#Construire la matrice des mots
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,25)






 