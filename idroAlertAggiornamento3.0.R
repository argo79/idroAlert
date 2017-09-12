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
#options(java.parameters = "-Xmx500m")
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
setwd("/home/argo/R-project/idroAlert1.0/")
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
   colnames(tabellaIdro2)<-c("Id","Fiume","Luogo","Url","Regione")
   tabellaIdro2$Differenza<-NA
   # Livello attuale
   tabellaIdro2$Adesso<-NA
   tabellaIdro2$Data<-NA
   tabellaIdro2$Ora<-NA
   # Livello minimo
   tabellaIdro2$Minimo<-NA
   tabellaIdro2$DataMin<-NA
   tabellaIdro2$OraMin<-NA
   # Livello massimo
   tabellaIdro2$Massimo<-NA
   tabellaIdro2$DataMax<-NA
   tabellaIdro2$OraMax<-NA
   tabellaIdro2<-tabellaIdro2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
   print("Sono riuscito ad aprire un file con i parametri dei fiumi.")
}
if (!file.exists("./conf/tabellaIdroDB.csv")) {
   print("Il file con i parmateri dei fiumi non esiste!")
   tabellaIdro<-data.frame(listaFiumi)
   tabellaIdro<-t(tabellaIdro)
   tabellaIdro2<-data.frame(tabellaIdro,row.names = NULL)
   colnames(tabellaIdro2)<-c("Id","Fiume","Luogo","Url","Regione")
   tabellaIdro2$Differenza<-NA
   # Livello attuale
   tabellaIdro2$Adesso<-NA
   tabellaIdro2$Data<-NA
   tabellaIdro2$Ora<-NA
   # Livello minimo
   tabellaIdro2$Minimo<-NA
   tabellaIdro2$DataMin<-NA
   tabellaIdro2$OraMin<-NA
   # Livello massimo
   tabellaIdro2$Massimo<-NA
   tabellaIdro2$DataMax<-NA
   tabellaIdro2$OraMax<-NA
   tabellaIdro2<-tabellaIdro2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
}
lista<-c(1:length(tabellaIdro2$Differenza))
idFiume<-c(as.numeric(tabellaIdro2$Id))
fiumi<-c(as.character(tabellaIdro2$Fiume))
luoghi<-c(as.character(tabellaIdro2$Luogo))
urls<-c(as.character(tabellaIdro2$Url))
livMin<-c(as.numeric(tabellaIdro2$Minimo))
dataXMin<-(as.character(tabellaIdro2$DataMin))
oraXMin<-c(as.character(tabellaIdro2$OraMin))
livMax<-c(as.numeric(tabellaIdro2$Massimo))
dataXMax<-c(as.character(tabellaIdro2$DataMax))
oraXMax<-c(as.character(tabellaIdro2$OraMax))
regione<-c(as.character(tabellaIdro2$Regione))
#write.table(tabellaIdro2,file="./idrometri/tabellaIdroDB.csv",row.names = FALSE,sep=",")
if (!file.exists("./report/tabellaIdroRep.csv")) {
   print("Il file con report dell'ultima scansione non esiste!")
   tabellaIdroBak<-tabellaIdro2
} else {
   tabellaIdroBak<-read.csv("./report/tabellaIdroRep.csv",sep=";")
   print("Il file con report dell'ultima scansione è stato caricato!")
}

#################################################
### CREO TABELLA IDROMETRI CON VALORE ATTUALE ###
#################################################
n=15
for (n in lista) {
   url<-listaFiumi[[n]][[4]]
   fiume<-listaFiumi[[n]][[2]]
   idFiume<-listaFiumi[[n]][[1]]
   idrometro<-listaFiumi[[n]][[3]]
   Sys.sleep(delay)
#  
#   canPingSite <- function(test.site) {
#      !as.logical(system(paste("ping -W 1 -c 1", test.site)))
#   }
#   t1<-canPingSite("www.arpa.veneto.it")
#   t2<-canPingSite("www.google.it")
#   urlcheck<-as.character(url)
#   t1<-canPingSite(urlcheck)
#   print(url)

   pagina<-getURL(url)
   
   ##########################################
   #####            VENETO             ######
   ##########################################
   
   if (listaFiumi[[n]][[5]]=="Veneto") {
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
         oraI<-as.numeric(substr(tableIdro$Ora,1,2))
         oraI<-oraI+1
         minutoI<-as.character(substr(tableIdro$Ora,4,5))
         secondi<-"00"
         #print(tableIdro$Livello)
         livello<-tableIdro[1,3]
         livelloM<-gsub("\\.", ",",livello)
         tableIdro$Data<-paste(anno,mese,giorno,sep="-")
         tableIdro$Ora<-paste(oraI,minutoI,secondi,sep=":")
         tableIdro$Livello<-livelloM
         #tableIdro<-tableIdro[2,]
         livelloOld<-tabellaIdroBak[n,7]
         livelloOld<-as.character(livelloOld)
         livelloOld<-as.numeric(gsub(",","\\.",livelloOld))
         differenza<-as.numeric(livello-livelloOld)
         if (is.na(differenza)) {
            differenza<-0
         }
         differenza<-format(round(differenza, 2), nsmall = 2)
         tabellaIdro2[n,6]<-differenza
         tabellaIdro2[n,7]<-livello
         tabellaIdro2[n,8]<-tableIdro$Data
         tabellaIdro2[n,9]<-tableIdro$Ora
         # Aggiorno i minimi e i massimi
         #         livelloMin<-gsub(",", "\\.",livelloConf)
         #         livelloMin<-as.numeric(livelloMin)
         livelloConfMin<-tabellaIdroBak[n,10]
         livelloConfMin<-as.character(livelloConfMin)
         livelloConfMin<-as.numeric(gsub(",","\\.",livelloConfMin))
         if (livello<=livelloConfMin || is.na(livelloConfMin)) {
            tabellaIdro2[n,10]<-livello
            tabellaIdro2[n,11]<-as.character(tableIdro$Data)
            tabellaIdro2[n,12]<-as.character(tableIdro$Ora)
         } else {
            tabellaIdro2[n,10]<-tabellaIdroBak[n,10]
            tabellaIdro2[n,11]<-as.character(tabellaIdroBak[n,11])
            tabellaIdro2[n,12]<-as.character(tabellaIdroBak[n,12])
         }
         livelloConfMax<-tabellaIdroBak[n,13]
         livelloConfMax<-as.character(livelloConfMax)
         livelloConfMax<-as.numeric(gsub(",","\\.",livelloConfMax))
         if (livello>=livelloConfMax || is.na(livelloConfMax)) {
            tabellaIdro2[n,13]<-livello
            tabellaIdro2[n,14]<-as.character(tableIdro$Data)
            tabellaIdro2[n,15]<-as.character(tableIdro$Ora)
         } else {
            tabellaIdro2[n,13]<-tabellaIdroBak[n,13]
            tabellaIdro2[n,14]<-as.character(tabellaIdroBak[n,14])
            tabellaIdro2[n,15]<-as.character(tabellaIdroBak[n,15])
         }
         # prova<-7-5
         
         # tableIdro<-tableIdro[1,c(4,5,6)]
         nomeFile<-paste(fiume,idrometro,sep="-")
         nomeFile<-paste("./report/fiumi/Veneto/",nomeFile,".csv",sep="")
         #stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"segna",livello,"m.")
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"segna",livello,"m. Alle",oraI,"e",minutoI,"minuti del giorno",giorno,mese,anno,"e presenta una differenza di",differenza,"m.")
         print(stampa)
         write.table(tableIdro,file=nomeFile,row.names = FALSE,col.names = FALSE,sep=";",append=TRUE,quote=FALSE)
      }
      else {
         out <- FALSE
         livello<-NA
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"non è raggiungibile.")
         print(stampa)
      }   
   }
   
   ##########################################
   #####            FRIULI             ######
   ##########################################
   
   if (listaFiumi[[n]][[5]]=="Friuli") {
      grepl("(Idrometro)",pagina)
      if (grepl("(Idrometro)",pagina)==TRUE) {
         out <- TRUE
         pagina<-read_html(url)
         #pagina
         pagina2<-html_nodes(pagina,"table")
         #pagina2
         pagina3<-html_table(pagina2[1],fill=TRUE)
         pagina4<-data.frame(pagina3)
         paginaSensori<-pagina4[-1,]
         listaSensori<-c(paginaSensori[,1])
         numeroSensori<-c(1:length(listaSensori))
         if (length(listaSensori)<1) {
            stampa<-paste("Niente da fare, non ci sono sensori per ",prefisso,"-",stazione,".",sep="")
            print(stampa)
         }
         else {
             s<-1
            for (s in numeroSensori) {
               sensore<-listaSensori[s]
               if (sensore=="Idrometro") {
                  # IDROMETRO
                  dataEora<-paginaSensori[s,2]
                  dataEora<-strsplit(dataEora," - ")
                  dataEora<-dataEora[[1]][[2]]
                  dataEora<-strsplit(dataEora," ore ")
                  dataX<-dataEora[[1]][[1]]
                  oraX<-dataEora[[1]][[2]]
                  livello<-paginaSensori[s,2]
                  idrometroVec<-strsplit(livello, " - ")
                  livello<-idrometroVec[[1]][[1]]
                  livello<-as.numeric(gsub("m","",livello,perl=TRUE,fixed=FALSE))
                  livelloN<-gsub("\\.", ",",livello)
                  # tabellaStazioni[n,7]<-idrometro
               }
               else {
                  # print("Non c'è niente da fare. Non ci sono idrometri.")
               }
            }
            data<-strsplit(dataX[[1]][[1]],"/")
            orario<-strsplit(oraX[[1]][[1]],":")
            anno<-data[[1]][[3]]
            mese<-data[[1]][[2]]
            giorno<-data[[1]][[1]]
            ora<-orario[[1]][[1]]
            minuto<-orario[[1]][[2]]
            secondi<-"00"
            # creo la tabella di riferimento temporanea che salva il valore attuale del fiume
            tableIdro<-pagina4
            tableIdro$Data<-paste(anno,mese,giorno,sep="-")
            tableIdro$Ora<-paste(ora,minuto,secondi,sep=":")
            tableIdro$Livello<-livello
            dataI<-paste(anno,mese,giorno,sep="-")
            oraI<-paste(ora,minuto,secondi,sep=":")
            livelloOld<-tabellaIdroBak[n,7]
            livelloOld<-as.character(livelloOld)
            livelloOld<-as.numeric(gsub(",","\\.",livelloOld))
            differenza<-as.numeric(livello-livelloOld)
            if (is.na(differenza)) {
               differenza<-0
            }
            differenza<-format(round(differenza, 2), nsmall = 2)
            tabellaIdro2[n,6]<-differenza
            tabellaIdro2[n,7]<-livello
            tabellaIdro2[n,8]<-dataI
            tabellaIdro2[n,9]<-oraI
            # Aggiorno i minimi e i massimi
            livelloConfMin<-(tabellaIdroBak[n,10])
            livelloConfMin<-as.character(livelloConfMin)
            livelloConfMin<-as.numeric(gsub(",","\\.",livelloConfMin))
            if (livello<=livelloConfMin || is.na(livelloConfMin)) {
               tabellaIdro2[n,10]<-livello
               tabellaIdro2[n,11]<-as.character(dataI)
               tabellaIdro2[n,12]<-as.character(oraI)
            } else {
               tabellaIdro2[n,10]<-tabellaIdroBak[n,10]
               tabellaIdro2[n,11]<-as.character(tabellaIdroBak[n,11])
               tabellaIdro2[n,12]<-as.character(tabellaIdroBak[n,12])
            }
            livelloConfMax<-(tabellaIdroBak[n,13])
            livelloConfMax<-as.character(livelloConfMax)
            livelloConfMax<-as.numeric(gsub(",","\\.",livelloConfMax))
            if (livello>=livelloConfMax || is.na(livelloConfMax)) {
               tabellaIdro2[n,13]<-livello
               tabellaIdro2[n,14]<-as.character(dataI)
               tabellaIdro2[n,15]<-as.character(oraI)
            } else {
               tabellaIdro2[n,13]<-tabellaIdroBak[n,13]
               tabellaIdro2[n,14]<-as.character(tabellaIdroBak[n,14])
               tabellaIdro2[n,15]<-as.character(tabellaIdroBak[n,15])
            }
            tableIdro<-tableIdro[1,c(4,5,6)]
            #tableIdro<-tableIdro[2,]
            nomeFile<-paste(fiume,idrometro,sep="-")
            nomeFile<-paste("./report/fiumi/Friuli/",nomeFile,".csv",sep="")
            stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"segna",livello,"m. Alle",ora,"e",minuto,"minuti del giorno",giorno,mese,anno,"e presenta una differenza di",differenza,"m.")
            print(stampa)
            write.table(tableIdro,file=nomeFile,row.names = FALSE,col.names = FALSE,sep=";",append=TRUE, quote=FALSE)
         }
      }
      else {
         livello<-NA
         stampa<-paste("L'idrometro del fiume",fiume,"a",listaFiumi[[n]][[3]],"non è raggiungibile.")
         print(stampa)
      }
   }
}
print("Tutto è andato bene.")
write.table(tabellaIdro2[,c(1:15)],file="./report/tabellaIdroRep.csv",row.names = FALSE,sep=";")
print("Ho salvato il file tabellaIdroRep.csv con i valori aggiornati di tutti gli idrometri.")
##############################
### FINE TABELLA IDROMETRI ###
##############################
