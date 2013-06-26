
df <- read.csv("/media/Data/Dropbox/Thèse/données ministère intérieur/partielle 2013/47 Leg part Villeneuve sur Lot T1.csv", sep=";", dec=",", stringsAsFactors=FALSE)
df$Canton <- sprintf("%02d", df$Canton)
df$CodeCommune <- paste("47",sprintf("%03d", df$CodeCommune),sep="")
df$CodeBV <- paste(df$CodeCommune, sprintf("%04d", df$BV), sep="")
df[,paste(names(df)[9:25], ".ins", sep="")] <- df[,9:25] / df$Inscrits * 100
df[,paste(names(df)[9:25], ".exp", sep="")] <- df[,9:25] / df$Exprimés * 100
df$Abstention.ins <- (df[,5] - df[,6])/df$Inscrits
df <- df[-which(grepl("Nekkaz",names(df)))]
library(ggplot2)
ggplot(df, aes(x=Costes.exp, y=Bousquet.exp)) + geom_point(aes(size=Inscrits), shape=1) + geom_smooth(method="lm") + theme_bw()
ggplot(df, aes(x=Costes.exp, y=Barral.exp)) + geom_point(aes(size=Inscrits), shape=1) + geom_smooth(method="lm") + theme_bw()
ggplot(df, aes(x=Bousquet.exp, y=Barral.exp)) + geom_point(aes(size=Inscrits), shape=1) + geom_smooth(method="lm") + theme_bw()
library(FactoMineR)
acp <- PCA(df[,42:58], row.w=df$Inscrits, graph=FALSE)
plot(acp, choix="var", lim.cos2.var=0.25)
load("/media/Data/Dropbox/Thèse/données propres/cartes/communes.Rdata")
circo4703 <- communes[communes@data$CodeInsee %in% unique(df$CodeCommune),]
library(cartoPOO)
# tentative de carte OSM

library(rgdal)
library(OpenStreetMap)
library(proj4)
b <- project(bbox(circo4703), proj=proj4string(circo4703), inv=TRUE)
m <- openmap(c(b[2,2], b[1,1]), c(b[1,2], b[2,1]), type="bing", mergeTiles=TRUE)
plot(m)
mproj <- openproj(m, projection=proj4string(circo4703))
plot(mproj)
# plot(circo4703, add=TRUE)

dfcom <- ddply(df, .(CodeCommune), summarise, Bousquet = sum(Bousquet)/sum(Exprimés))
plot(choropleth(map(circo4703, dfcom), varname="Bousquet", sp.key="CodeInsee", data.key="CodeCommune", cut.method="quantile", border="transparent", palette="Blues",alpha=0.5), add=TRUE)

dfcom <- ddply(df, .(CodeCommune), summarise, Costes = sum(Costes)/sum(Exprimés))
plot(mproj)
plot(choropleth(map(circo4703, dfcom), varname="Costes", sp.key="CodeInsee", data.key="CodeCommune", cut.method="quantile", border="transparent",palette="Blues", alpha=0.5), add=TRUE)
dfcom <- ddply(df, .(CodeCommune), summarise, Barral = sum(Barral)/sum(Exprimés))
plot(mproj)
plot(choropleth(map(circo4703, dfcom), varname="Barral", sp.key="CodeInsee", data.key="CodeCommune", cut.method="quantile", palette="Blues", border="transparent", alpha=0.5), add=TRUE)
dfcom <- ddply(df, .(CodeCommune), summarise, Abstention = weighted.mean(Abstention.ins, Inscrits))
plot(mproj)
plot(choropleth(map(circo4703, dfcom), varname="Abstention", sp.key="CodeInsee", data.key="CodeCommune", cut.method="quantile", palette="Blues",border="transparent", alpha=0.5),add=TRUE)

load("/media/Data/Dropbox/Thèse/données ministère intérieur/par BV 2011/Cant2011T1BV.Rdata")
Cant2011T1BV$CodeInsee <- paste(Cant2011T1BV$Dep, Cant2011T1BV$Comm, sep="")
Cant2011T1BV$CodeBV <- paste(Cant2011T1BV$CodeInsee, Cant2011T1BV$BV, sep="")
load("/media/Data/Dropbox/Thèse/données ministère intérieur/par BV 2011/Cant2011T2BV.Rdata")
Cant2011T2BV$CodeInsee <- paste(Cant2011T2BV$Dep, Cant2011T2BV$Comm, sep="")
Cant2011T2BV$CodeBV <- paste(Cant2011T2BV$CodeInsee, Cant2011T2BV$BV, sep="")

# cantonales 2011, score FN dans les cantons renouvelables où il est présent
sum(Cant2011T1BV[(Cant2011T1BV$CodeBV %in% df$CodeBV) & (Cant2011T1BV$FN > 0), "FN"]) / sum(Cant2011T1BV[(Cant2011T1BV$CodeBV %in% df$CodeBV) & (Cant2011T1BV$FN > 0), "Inscrits"])
sum(Cant2011T1BV[(Cant2011T1BV$CodeBV %in% df$CodeBV) & (Cant2011T1BV$FN > 0), "FN"]) / sum(Cant2011T1BV[(Cant2011T1BV$CodeBV %in% df$CodeBV) & (Cant2011T1BV$FN > 0), "Exprimés"])

# scores dans le même périmètre en 2013
sum(df[df$CodeBV %in% Cant2011T1BV[Cant2011T1BV$FN > 0, "CodeBV"], "Bousquet"]) / sum(df[df$CodeBV %in% Cant2011T1BV[Cant2011T1BV$FN > 0, "CodeBV"], "Inscrits"])
sum(df[df$CodeBV %in% Cant2011T1BV[Cant2011T1BV$FN > 0, "CodeBV"], "Bousquet"]) / sum(df[df$CodeBV %in% Cant2011T1BV[Cant2011T1BV$FN > 0, "CodeBV"], "Exprimés"])

# progression
index <- Cant2011T1BV[(Cant2011T1BV$CodeBV %in% df$CodeBV) & (Cant2011T1BV$FN > 0), "CodeBV"]
d <- data.frame(C11 = Cant2011T1BV[match(index, Cant2011T1BV$CodeBV), "FN.ins"], C11PS =  Cant2011T1BV[match(index, Cant2011T1BV$CodeBV), "SOC.ins"] + Cant2011T1BV[match(index, Cant2011T1BV$CodeBV), "DVG.ins"], L13 = df[match(index, df$CodeBV), "Bousquet.ins"], BV = index, Inscrits = df[match(index, df$CodeBV), "Inscrits"])
ggplot(d, aes(x=C11, y=L13)) + geom_point(aes(size=Inscrits), shape=1) + geom_smooth(method="lm", aes(weight=Inscrits)) + geom_abline(slope=1) + xlab("FN Cantonales 2011, % inscrits") + ylab("FN législative 2013, % inscrits") +  theme_bw()
ggplot(d, aes(x=C11PS, y=C11)) + geom_point(aes(size=Inscrits), shape=1) + geom_smooth(method="lm", aes(weight=Inscrits)) + xlab("PS Cantonales 2011, % inscrits") + ylab("FN Cantonales 2011, % inscrits") +  theme_bw()

# progression entre deux tours 2011
index2 <- index[index %in% Cant2011T2BV[Cant2011T2BV$FN > 0, "CodeBV"]]
df <- data.frame(FN1 = Cant2011T1BV[match(index2, Cant2011T1BV$CodeBV),"FN.ins"], FN2 = Cant2011T2BV[match(index2, Cant2011T2BV$CodeBV), "FN.ins"], Inscrits= Cant2011T1BV[match(index2, Cant2011T1BV$CodeBV), "Inscrits"])
ggplot(df, aes(x=FN1, y=FN2-FN1)) + geom_point(aes(size=Inscrits), shape=1) + geom_smooth(method="lm", aes(weight=Inscrits)) + geom_abline(slope=1) + xlab("Vote FN, 1er tour Cantonales 2011, en % des inscrits") + ylab("Vote FN, 2e tour Cantonales 2011, en % des inscrits") + theme_bw()
sum(Cant2011T1BV[match(index2, Cant2011T1BV$CodeBV),"FN"]) / sum(Cant2011T1BV[match(index2, Cant2011T1BV$CodeBV),"Exprimés"]) * 100 
sum(Cant2011T2BV[match(index2, Cant2011T2BV$CodeBV), "FN"]) / sum(Cant2011T2BV[match(index2, Cant2011T2BV$CodeBV), "Exprimés"]) * 100
