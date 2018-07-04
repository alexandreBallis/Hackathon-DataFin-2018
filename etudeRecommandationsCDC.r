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
phrase <-  "faire �voluer le r�gime de retraite des membres 
vers un r�gime � cotisations d�finies,n'engageant pas l'Etat 
au-del� du financement initialement consenti et applicable 
� l'ensemble des pensions non encore liquid�es"
phrase1 <- "le r�gime de retraite doit �voluer sur la base de 
cotisations d�finies, qui n'engage pas l'Etat au-del� du financement 
consenti au d�part et applicable � l'ensemble des pensions non encore pay�es"
phrase2 <- "ma�triser rigoureusement les charges de fonctionnement des CLCC
pour r�tablir leur �quilibre financier et leur capacit� � investir, 
en particulier dans les centres les plus fragiles."
print(stringdist(phrase,phrase1,method="jaccard"))
#######################################################################
#texte <- tolower(texte)
#texte <- str_replace(texte,"�","e")
#texte.words <- unlist(strsplit(texte,"[[:blank:].-]+")) #d�composition du texte en mots 
#texte.words <- na.omit(str_match(texte.words,"[^0-9]+")) #Nettoyage du texte des chiffres mais renvoie un type matrix
#texte.words <- as.character(na.omit(str_match(texte.words,"[a-zA-z����]{3,18}"))) #Enl�vement de mots de liaison non significatifs

########################### package dplyr ############################################
# transfomation du books_cdc : colonne 3 : titres des recommandations nettoy�s par textClean()
# description du books_cdc
# col 2 : Type de publication / col 3 : Titre / col 4 : recommandations / col 5 : destinataire 1
library(dplyr)
df <- books_cdc
df[,4] <- sapply(1:nrow(books_cdc),function(i) {books_cdc[i,4] <- tolower(books_cdc[i,4])})
#df[,4] <- str_c(strsplit(df[,4],"[[:blank:].-]+"),collapse=" ")
# manipulation du type tibble du package dplyr. Ce type est un data.frame sp�cial dont les variables peuvent �tre des listes !! 
df_bb <- (tibble(publication=df[,2],titre=as.list(df[,3]),destination=df[,5])) #construction d'un data.frame sp�cial (
xtitres <- lapply(df_bb$titre,textClean) #nettoyage des titres
xtitres <- lapply(xtitres,function(i) {str_c(i,collapse=" ")})
df_bb$titre <- unlist(xtitres)
df_bb <- unique(df_bb)
writeout(as.data.frame(df_bb),"titresCDC",TRUE)



					