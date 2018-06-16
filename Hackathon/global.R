library(shiny)
# library(grid)
#library(gridExtra)
# library(plyr)
# library(Cairo)
# library(plotly)
# library(ggplot2)
# library(scales)

options(scipen = 999)

lire_liste_fichiers = function(fichier){
  if (grepl('PLR', fichier)){
    donnees = read.csv(fichier, stringsAsFactors = F, dec = ",", skip = 4)
    } else{
      donnees = read.csv(fichier, stringsAsFactors = F, dec = ",")
  }
  return(assign(fichier, donnees))
}



# CGE
cge = read.csv("./Donnees/CGE/2012 - 2017_Balances des comptes de l'etat.csv", 
                stringsAsFactors = F, dec = ",")
cge$Programme = gsub('\\s+', '', cge$Programme)
cge$Programme = gsub('^0', '', cge$Programme)

col_balance = colnames(cge)[grepl('Balance.Sortie', colnames(cge))]
for (c in col_balance){
  cge[, c] = gsub(' ', '', cge[, c])
  cge[, c] = gsub(',', '.', cge[, c])
  cge[, c] = as.numeric(cge[,c])
}
annees_col_balance = as.numeric(gsub('Balance.Sortie.(.*)', '\\1', col_balance))

# PLR
plr_files = list.files('./donnees/PLR', full.names = T)
plr = Map(lire_liste_fichiers, plr_files)
names(plr) = gsub('.*PLR([0-9]+)-.*', '\\1', names(plr))

# RAP
rap_files = list.files('./donnees/RAP', full.names = T)
rap = Map(lire_liste_fichiers, rap_files)
names(rap) = gsub('.*PERF_SSI_RAP_([0-9]+)_.*', '\\1', names(rap))


