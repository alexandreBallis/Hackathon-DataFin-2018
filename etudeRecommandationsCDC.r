 #ETUDE DES RECOMMANDATIONS DE LA COUR DES COMPTES#
 #Necessite le lancement de textClean.r pour la donction textClean()
 fichierSelectionne <- "TabRecommandationsCDC.csv"
 books_cdc <- read.csv(fichierSelectionne,sep=";",header=T,stringsAsFactors=FALSE) 
 colRecommandations <- books_cdc$Libellé.recommandation
 colTypePubli       <- books_cdc$Type.de.publication 
 # transformation du data.frame par éclatement des phrases de recommandations #
 
 df <- books_cdc %>% 
		unnest(Libellé.recommandation=strsplit(Libellé.recommandation,"[),.' ]"))
 #Nettoyage du data frame des recommandations de la cour des comptes
 cdc_count <- books_cdc%>% 
	mutate(text = stringr::str_replace_all(.$Libellé.recommandation,"’", " ")) %>%
	unnest_tokens(Libellé.recommandation, text) %>%
    filter(!Libellé.recommandation %in% stopwords('fr')) %>%
	count(Libellé.recommandation, sort = TRUE)
	############## nettoyage terminé ###############################
 pool <- textClean(str_c(colRecommandations,sep=" ",collapse=" "))
 pool <- str_replace(pool,"é","e")
 ###  chargements des mots de liaisons inutiles ###
 library("stopwords") 
 motsinutiles <- stopwords(language = "fr") #classe caracteres
 mi <- motsinutiles #saisie simplifié
 pool <- setdiff(pool,motsinutiles) #bassin des mots du texte
 ###  fin procedure   mots de liaisons inutiles ###
 poool <- unique(pool)
 #strindist(mot1,mot2,method="cosine")
 x1 <- stringdist("valorisation",poool,method="cosine")
 x2 <- stringdist("valorisation",poool,method="lv")
 x3 <- stringdist("valorisation",poool,method="jaccard")
 x4 <- stringdist("valorisation",poool,method="qgram")
 #data.frame Performances de similarité textuelle en fonction des méthodes
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
#texte <- str_replace(texte,"é","e")
#texte.words <- unlist(strsplit(texte,"[[:blank:].-]+")) #décomposition du texte en mots 
#texte.words <- na.omit(str_match(texte.words,"[^0-9]+")) #Nettoyage du texte des chiffres mais renvoie un type matrix
#texte.words <- as.character(na.omit(str_match(texte.words,"[a-zA-zéèçà]{3,18}"))) #Enlèvement de mots de liaison non significatifs

df <- books_cdc
df[,4] <- sapply(1:nrow(books_cdc),function(i) {books_cdc[i,4] <- tolower(books_cdc[i,4])})
df[,4] <- str_c(strsplit(df[,4],"[[:blank:].-]+"),collapse=" ")

					
