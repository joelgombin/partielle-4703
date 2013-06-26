# import et préparation des données 
df <- read.csv("47 Leg part Villeneuve sur Lot T1.csv", sep=";", dec=",", stringsAsFactors=FALSE)
names(df)[8] <- "BlancsNuls"
df$Canton <- sprintf("%02d", df$Canton)
df$CodeCommune <- paste("47",sprintf("%03d", df$CodeCommune),sep="")
df$CodeBV <- paste(df$CodeCommune, sprintf("%04d", df$BV), sep="")
df[,paste(names(df)[c(7:8,10:26)], ".ins", sep="")] <- df[,c(7:8,10:26)] / df$Inscrits * 100
df[,paste(names(df)[10:26], ".exp", sep="")] <- df[,10:26] / df$Exprimés * 100
df <- df[-which(grepl("Nekkaz",names(df)))]

df2 <- read.csv("47 Leg part Villeneuve sur Lot T2.csv", sep=";", dec=",", stringsAsFactors=FALSE)
df2$CodeCommune <- paste("47",sprintf("%03d", df2$CodeCommune),sep="")
df2$CodeBV <- paste(df2$CodeCommune, sprintf("%04d", df2$BV), sep="")
df2$Canton <- df[match(df2$CodeBV, df$CodeBV),"Canton"]
names(df2)[c(4:10)] <- c("Inscrits2", "Votants2", "Abstention2", "BlancsNuls2", "Exprimés2","Bousquet2","Costes2")


df2$Abstention2.ins <- df2$Abstention2 / df2$Inscrits2 * 100
df2$BlancsNuls2.ins <- df2$BlancsNuls2 / df2$Inscrits2 * 100
df2$Bousquet2.ins <- df2$Bousquet2 / df2$Inscrits2 * 100
df2$Costes2.ins <- df2$Costes2 / df2$Inscrits2 * 100
df2$Bousquet2.exp <- df2$Bousquet2 / df2$Exprimés2 * 100
df2$Costes2.exp <- df2$Costes2 / df2$Exprimés2 * 100

df <- merge(df, df2[,c(4:11,13:18)], "CodeBV")

# dans certains bureaux, le nombre d'inscrits varie légèrement entre les deux tours (maximum : 8 inscrits de variation). On corrige cela en corrigeant artificiellement le nombre d'abstentionnistes au second tour.

df$Abstention2 <- df$Abstention2 - (df$Inscrits2 - df$Inscrits)
df$Inscrits2 <- df$Inscrits2 - (df$Inscrits2 - df$Inscrits)


# fusion de tous les petits candidats
df$autres <- df$Kichi + df$Raphael + df$Asselineau + df$Geyres + df$Frison + df$Levieux + df$Garcia.Luna + df$Miguet + df$Lebreton + df$Garay
df$autres.ins <- df$autres / df$Inscrits * 100
df$autres.exp <- df$autres / df$Exprimés * 100


# installation des packages nécessaires
lapply(c("mvtnorm", "msm", "tmvtnorm", "ellipse", "plotrix", "sp", "ucminf", "cubature", "mnormt", "foreach", "eiPack"), function(x) install.packages(x))
install.packages("ei",repos="http://r.iq.harvard.edu") 
library(ei)

# spécification du modèle
formula <- cbind(Abstention2, BlancsNuls2, Bousquet2, Costes2) ~ cbind(Abstention, BlancsNuls, autres, Bousquet, Loiseau, Feuillas, Costes, Carpentier, Barral)

# on fait tourner le modèle 
dbuf <- ei(formula=formula, data=df, sample=50000)

# on crée une matrice pour stocker les estimations
tour1 <- c("Abstention","BlancsNuls", "autres", "Bousquet", "Loiseau", "Feuillas", "Costes", "Carpentier", "Barral")
tour2 <- c("Abstention2", "BlancsNuls2", "Bousquet2", "Costes2")
df <- array(dim=c(length(tour1), length(tour2)))
dimnames(df)[[1]] <- names(res)[c(34, 5,7:13)]
dimnames(df)[[2]] <- names(res)[c(35,16,18,19)]