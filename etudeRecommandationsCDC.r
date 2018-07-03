 #ETUDE DES RECOMMANDATIONS DE LA COUR DES COMPTES#
 #Necessite le lancement de textClean.r pour la donction textClean()
 fichierSelectionne <- "TabRecommandationsCDC.csv"
 books_cdc <- read.csv(fichierSelectionne,sep=";",header=T,stringsAsFactors=FALSE) 
 colRecommandations <- books_cdc$Libell�.recommandation
 colTypePubli       <- books_cdc$Type.de.publication 
 # transformation du data.frame par �clatement des phrases de recommandations #
 #exemple:
 df <- data.frame(x=letters[runif(3,min=1,max=26)],
					phrases=c("mon.chat a peur","caro.line n'est plus, d�put�e","olive.vert, pas mure"),
					stringsAsFactors=FALSE)
 df %>% unnest(phrases=strsplit(phrases,"[,.' ]"))
 #fin exemple
 df <- books_cdc %>% 
		unnest(Libell�.recommandation=strsplit(Libell�.recommandation,"[),.' ]"))
 #Nettoyage du data frame des recommandations de la cour des comptes
 cdc_count <- books_cdc%>% 
	mutate(text = stringr::str_replace_all(.$Libell�.recommandation,"�", " ")) %>%
	unnest_tokens(Libell�.recommandation, text) %>%
    filter(!Libell�.recommandation %in% stopwords('fr')) %>%
	count(Libell�.recommandation, sort = TRUE)
	############## nettoyage termin� ###############################
 pool <- textClean(str_c(colRecommandations,sep=" ",collapse=" "))
 pool <- str_replace(pool,"�","e")
 ###  chargements des mots de liaisons inutiles ###
 library("stopwords") 
 motsinutiles <- stopwords(language = "fr") #classe caracteres
 mi <- motsinutiles #saisie simplifi�
 pool <- setdiff(pool,motsinutiles) #bassin des mots du texte
 ###  fin procedure   mots de liaisons inutiles ###
 poool <- unique(pool)
 #strindist(mot1,mot2,method="cosine")
 x1 <- stringdist("valorisation",poool,method="cosine")
 x2 <- stringdist("valorisation",poool,method="lv")
 x3 <- stringdist("valorisation",poool,method="jaccard")
 x4 <- stringdist("valorisation",poool,method="qgram")
 #data.frame Performances de similarit� textuelle en fonction des m�thodes
print("cosine:")
print(x1[order(x1)[1:6]])
print(str_c(t(poool[order(x1)][1:6])),collapse=" - ")
print("levenstein:")
print(x2[order(x2)[1:6]])
print(str_c(t(poool[order(x2)][1:6])),collapse=" - ")
print("jaccard:")
print(x3[order(x3)[1:6]])
print(str_c(t(poool[order(x3)][1:6])),collapse=" - ")
print("qgram:")
print(x4[order(x4)[1:6]])
print(str_c(t(poool[order(x4)][1:6])),collapse=" - ")
#######################################################################
#texte <- tolower(texte)
#texte <- str_replace(texte,"�","e")
#texte.words <- unlist(strsplit(texte,"[[:blank:].-]+")) #d�composition du texte en mots 
#texte.words <- na.omit(str_match(texte.words,"[^0-9]+")) #Nettoyage du texte des chiffres mais renvoie un type matrix
#texte.words <- as.character(na.omit(str_match(texte.words,"[a-zA-z����]{3,18}"))) #Enl�vement de mots de liaison non significatifs

df <- books_cdc
df[,4] <- sapply(1:nrow(books_cdc),function(i) {books_cdc[i,4] <- tolower(books_cdc[i,4])})
df[,4] <- str_c(strsplit(df[,4],"[[:blank:].-]+"),collapse=" ")

					