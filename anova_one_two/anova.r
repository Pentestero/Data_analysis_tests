#INITIALISATION
nb_groupe <- NA
while (is.na(nb_groupe) || nb_groupe <= 1) {
  nb_groupe <- as.integer(readline(prompt = "Combien de groupes voulez-vous comparer ? (Minimum 2) : "))
  if (is.na(nb_groupe) || nb_groupe <= 1) {
    cat("Erreur : Entrez un nombre entier supérieur ou égal à 2.\n")
  }
}

donnees <- data.frame()
noms_utilises <- c()

#BOUCLE DE SAISIE
for (i in 1:nb_groupe) {
  
  #Gestion d'erreur pour le NOM
  nom <- ""
  repeat {
    nom <- trimws(readline(prompt = sprintf("Nom du groupe : ", i)))
    
    #Sécurité : vérifier si l'utilisateur n'a pas mis des virgules ou trop de chiffres dans le nom
    if (nchar(nom) == 0) {
      cat("Erreur : Le nom ne peut pas être vide.\n")
    } else if (grepl(",", nom)) {
      cat("Attention : Il semble que vous essayez d'entrer des valeurs à la place du nom. Entrez un nom simple (ex: M1).\n")
    } else if (nom %in% noms_utilises) {
      cat(sprintf("Erreur : Le nom '%s' existe déjà.\n", nom))
    } else {
      noms_utilises <- c(noms_utilises, nom)
      break
    }
  }
  
  #Gestion d'erreur pour les VALEURS
  valeurs <- NA
  repeat {
    saisie <- readline(prompt = sprintf("Valeurs pour '%s' (ex: 12 15 10) : ", nom))
    #Les valeurs peuvent etre séparées par des virgules ou juste des espaces
    saisie_nettoyee <- gsub(",", " ", saisie)
    chaines <- unlist(strsplit(trimws(saisie_nettoyee), "\\s+"))
    valeurs <- as.numeric(chaines)
    
    if (any(is.na(valeurs)) || length(valeurs) == 0) {
      cat("Saisie invalide : Entrez uniquement des nombres séparés par des espaces.\n")
    } else {
      break 
    }
  }
  
  cat(sprintf("-> Groupe '%s' validé.\n", nom))
  donnees <- rbind(donnees, data.frame(
    valeur = valeurs,
    groupe = factor(rep(nom, length(valeurs)))
  ))
}

#ANALYSE FINALE
cat("\n============================================\n")
cat("          RÉSULTATS DE L'ANOVA\n")
cat("============================================\n")

resultat_anova <- aov(valeur ~ groupe, data = donnees)
print(summary(resultat_anova))

#Extraction automatique de la P-Value pour interprétation
res_sum <- summary(resultat_anova)[[1]]
p_val <- res_sum[["Pr(>F)"]][1]

cat("\nConclusion : ")
if (p_val < 0.05) {
  cat("Différence significative (p < 0.05). Les moyennes sont différentes.\n")
} else {
  cat("Pas de différence significative (p > 0.05).\n")
}