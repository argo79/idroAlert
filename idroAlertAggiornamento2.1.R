library(dplyr)
library(htmltab)
library(readr)
library(rvest)
library(maptools)
library(ggplot2)
library(rgeos)
#library(rgdal)
#library(ggmap)
library(scales)
library(htmltab)
#options(java.parameters = "-Xmx8000m")
library(rJava)
library(mailR)
library(sendmailR)
library(RCurl)
library(xtable)
library(pander)

#############################################################################################
###                                                                                       ###
###                    FUNZIONA CON IL NUOVO SITO E LE STRINGHE                           ###
###                                                                                       ###
#############################################################################################

###   PARAMETRI DA CONFIGURARE   ###
setwd("/home/argo/workspace/R/idroAlert1.0/")
delay=0.75
# report<-1
if (file.exists("./conf/tabellaIdroDB.csv")) {
   tabellaIdro2<-read_csv("./conf/tabellaIdroDB.csv")
   if (length(tabellaIdro2)<0) {
      print("Il file con i parametri dei fiumi è vuoto!")
   }
   listaFiumi<-unlist(apply(tabellaIdro2, 1, list), recursive = FALSE)
   tabellaIdro<-data.frame(listaFiumi)
   tabellaIdro<-t(tabellaIdro)
   tabellaIdro2<-data.frame(tabellaIdro,row.names = NULL)
   colnames(tabellaIdro2)<-c("Id","Fiume","Luogo","Url","LivelloMinimo","Regione")
   tabellaIdro2$Adesso<-NA
   tabellaIdro2$Data<-NA
   tabellaIdro2$Ora<-NA
   tabellaIdro2<-tabellaIdro2[,c(1,2,3,4,5,7,6,8,9)]
   print("Sono riuscito ad aprire un file con i parametri dei fiumi.")
}
if (!file.exists("./conf/tabellaIdroDB.csv")) {
   print("Il file con i parmateri dei fiumi non esiste!")
   tabellaIdro<-data.frame(listaFiumi)
   tabellaIdro<-t(tabellaIdro)
   tabellaIdro2<-data.frame(tabellaIdro,row.names = NULL)
   colnames(tabellaIdro2)<-c("Id","Fiume","Luogo","Url","LivelloMinimo","Regione")
   tabellaIdro2$Adesso<-NA
   tabellaIdro2$Data<-NA
   tabellaIdro2$Ora<-NA
   tabellaIdro2<-tabellaIdro2[,c(1,2,3,4,5,7,6,8,9)]
}
lista<-c(1:length(tabellaIdro2$LivelloMinimo))
idFiume<-c(as.numeric(tabellaIdro2$Id))
fiumi<-c(as.character(tabellaIdro2$Fiume))
luoghi<-c(as.character(tabellaIdro2$Luogo))
urls<-c(as.character(tabellaIdro2$Url))
livMin<-c(as.numeric(tabellaIdro2$`Livello Minimo`))
regione<-c(as.character(tabellaIdro2$Regione))
#write.table(tabellaIdro2,file="./idrometri/tabellaIdroDB.csv",row.names = FALSE,sep=",")


#################################################
### CREO TABELLA IDROMETRI CON VALORE ATTUALE ###
#################################################

for (n in lista) {
   url<-listaFiumi[[n]][[4]]
   fiume<-listaFiumi[[n]][[2]]
   idFiume<-listaFiumi[[n]][[1]]
   idrometro<-listaFiumi[[n]][[3]]
   Sys.sleep(delay)
   pagina<-getURL(url)
   if (listaFiumi[[n]][[6]]=="Veneto") {
      grepl("<table>",pagina)
      if (grepl("<table>",pagina[1])==TRUE) {
         out <- TRUE
         tableIdro<-htmltab(doc = url, which="//*[@id='datiidrometeo']/table" )
         tableIdro$Livello<-as.numeric(tableIdro$`Livello idrometrico(m)`)
         tableIdro$Data<-as.character(tableIdro$Data)
         tableIdro<-tableIdro[1,c(1,2,4)]
         anno<-as.character(substr(tableIdro$Data,7,10))
         mese<-as.character(substr(tableIdro$Data,4,5))
         giorno<-as.character(substr(tableIdro$Data,1,2))
         oraI<-as.character(substr(tableIdro$Ora,1,2))
         minutoI<-as.character(substr(tableIdro$Ora,4,5))
         secondi<-"00"
         #print(tableIdro$Livello)
         livello<-tableIdro[1,3]
         livelloM<-gsub("\\.", ",",livello)
         tableIdro$Data<-paste(anno,mese,giorno,sep="-")
         tableIdro$Ora<-paste(oraI,minutoI,secondi,sep=":")
         tableIdro$Livello<-livelloM
         #tableIdro<-tableIdro[2,]
         tabellaIdro2[n,6]<-livello
         tabellaIdro2[n,8]<-tableIdro$Data
         tabellaIdro2[n,9]<-tableIdro$Ora
         nomeFile<-paste(fiume,idrometro,sep="-")
         nomeFile<-paste("./report/fiumi/Veneto/",nomeFile,".csv",sep="")
         #stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"segna",livello,"m.")
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"segna",livelloM,"m. Alle",oraI,"e",minutoI,"minuti del giorno",giorno,mese,anno,".")
         print(stampa)
         write.table(tableIdro,file=nomeFile,row.names = FALSE,col.names = FALSE,sep=";",quote=FALSE)
      }
      else {
         out <- FALSE
         livello<-NA
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"non è raggiungibile.")
         print(stampa)
      }   
   }
   if (listaFiumi[[n]][[6]]=="Friuli") {
      grepl("(Idrometro)",pagina)
      if (grepl("(Idrometro)",pagina)==TRUE) {
         out <- TRUE
         pagina<-read_html(url)
         #pagina
         pagina2<-html_nodes(pagina,"table")
         #pagina2
         pagina3<-html_table(pagina2[1],fill=TRUE)
         pagina4<-data.frame(pagina3)
         stringa<-(pagina4[2,2])
         # estraggo le stringhe dei valori numerici
         numeriIdro<-c(na.omit(as.numeric(unlist(strsplit(unlist(stringa), "[^0-9]+")))))
         #numeriIdro
         giorno<-numeriIdro[3]
         if (giorno<=9) {
            giorno<-paste("0",giorno,sep="")
         }
         mese<-numeriIdro[4]
         if (mese<=9) {
            mese<-paste("0",mese,sep="")
         }
         anno<-numeriIdro[5]
         ora<-numeriIdro[6]
         if (ora<=9) {
            ora<-paste("0",ora,sep="")
         }
         minuto<-numeriIdro[7]
         if (minuto<=9) {
            minuto<-paste("0",minuto,sep="")
         }
         secondi<-"00"
         livelloC<-substr(stringa,1,5)
         livelloC<-gsub("m","",livelloC)
         livelloN<-as.numeric(livelloC)
         livello<-gsub("\\.", ",",livelloN)
         # creo la tabella di riferimento temporanea che salva il valore attuale del fiume
         tableIdro<-pagina4
         tableIdro$Data<-paste(anno,mese,giorno,sep="-")
         tableIdro$Ora<-paste(ora,minuto,secondi,sep=":")
         tableIdro$Livello<-livello
         dataI<-paste(anno,mese,giorno,sep="-")
         oraI<-paste(ora,minuto,secondi,sep=":")
         tabellaIdro2[n,6]<-livelloN
         tabellaIdro2[n,8]<-dataI
         tabellaIdro2[n,9]<-oraI
         tableIdro<-tableIdro[1,c(4,5,6)]
         #tableIdro<-tableIdro[2,]
         nomeFile<-paste(fiume,idrometro,sep="-")
         nomeFile<-paste("./report/fiumi/Friuli/",nomeFile,".csv",sep="")
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"segna",livello,"m. Alle",ora,"e",minuto,"minuti del giorno",giorno,mese,anno,".")
         print(stampa)
         write.table(tableIdro,file=nomeFile,row.names = FALSE,col.names = FALSE,sep=";",quote=FALSE)
      }
      else {
         out <- FALSE
         livello<-NA
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"non è raggiungibile.")
         print(stampa)
      }   
   }
}
print("Tutto è andato bene.")
write.table(tabellaIdro2[,c(1:9)],file="./report/tabellaIdroRep.csv",row.names = FALSE,sep=",")
print("Ho salvato il file tabellaIdroRep.csv con i valori aggiornati di tutti gli idrometri.")
##############################
### FINE TABELLA IDROMETRI ###
##############################