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
options(java.parameters = "-Xmx8000m")
library(rJava)
library(mailR)
library(sendmailR)
library(RCurl)
library(xtable)
library(pander)
library(linkR)
library(xtable)

# Legge file utenti e manda mail in caso di Alert positivi o richiesta di report.
setwd("/home/argo/workspace/R/idroAlert1.0/")
######################
### TABELLA UTENTI ###
######################

# recupero vecchio file tabellaIdro2
tabellaIdro2<-read.csv("./report/tabellaIdroRep.csv")

elencoFiles<-list.files("*.conf",path="./conf/utenti/",include.dirs = TRUE)
lunghezzaFiles<-length(elencoFiles)
conteggio<-c(1:as.numeric(lunghezzaFiles))
elencoUtenti<-data.frame()

for (numero in conteggio) {
   nomeFile<-elencoFiles[numero]
   if (grepl(".conf",nomeFile)==TRUE) {
      nomeFileR<-gsub(".conf","",nomeFile)
      nomeUtente<-gsub("utente-","",nomeFileR)
      elencoUtenti<-c(elencoUtenti,nomeUtente)
   }
}

# recupero file degli utenti
#elencoUtenti<-c("Melloni","Bubu")
# nomeUt<-2
nomeUtente<-as.character(elencoUtenti[nomeUt])
for (nomeUtente in as.character(elencoUtenti)) {
   nomeFileUtente<-paste("utente-",nomeUtente,".conf",sep="")
   nomeFileUtente<-paste("./conf/utenti/",nomeFileUtente,sep="")
   listaProfile<-read.csv(file=nomeFileUtente,
                          sep="|",na.strings="",
                          fill=TRUE,blank.lines.skip=TRUE,
                          stringsAsFactors = FALSE)
   # write.table(listaProfile,nomeFileUtente,sep="|",dec=",",row.names=FALSE,col.names = TRUE)
   # listaProfile[]<-lapply(listaProfile, as.character)
   # nome utente
   utente<-as.character(listaProfile[1,2])
   print(utente)
   # id utente
   idut<-as.character(listaProfile[1,1])
   idut<-as.numeric(idut)
   # mail utente
   to<-as.character(listaProfile[1,4])
   reportUt<-as.numeric(as.character(listaProfile[1,3]))
   if (is.na(reportUt)==TRUE) {
      reportUt<-0
   }
   numeroAlert<-as.numeric(as.character(listaProfile[1,5]))
   if (is.na(numeroAlert)==TRUE) {
      numeroAlert<-0
   }
   numeroReport<-as.numeric(as.character(listaProfile[1,6]))
   if (is.na(numeroReport)==TRUE) {
      numeroReport<-0
   }
   # from<-"<alert.idrometri@gmx.com>"
   from<-"<alert.idrometri@gmail.com>"
   # to<-c("<melloni6@gmail.com>","<christian.posta@libero.it>")
   # "<gazzino.simone@gmail.com>"
   userlogin="alert.idrometri"
   password="idroalert77"
   oggetto<-paste("ALERT Idrometro fiumi.")
   subject<-"Alert Idrometri"
   # lista<-c(1:length(tabellaIdro2$LivelloMinimo))
   idFiume<-c(as.numeric(tabellaIdro2$Id))
   fiumi<-c(as.character(tabellaIdro2$Fiume))
   luoghi<-c(as.character(tabellaIdro2$Luogo))
   urls<-c(as.character(tabellaIdro2$Url))
   # livMin<-c(as.numeric(tabellaIdro2$`Livello Minimo`))
   regione<-c(as.character(tabellaIdro2$Regione))
   # listaProfile[1,21]
   # length(listaProfile[2,])
   
   tabellaFiumi<-listaProfile
   tabellaFiumi<-tabellaFiumi[c(-(1:2)),]
   colnames(tabellaFiumi)<-c("ID","Fiume","Luogo","ASopra","ASotto","Aggiornamento")
   
   #idut=1
   #n<-45
   # conta il numero di fiumi
   conteggio<-c(1:length(tabellaFiumi$Luogo))
   length(conteggio)
   if (as.numeric(length(conteggio))<=0) {
      # sono finiti i fiumi dell'utente
      messaggio<-paste("Non ci sono fiumi per l'utente ",utente,sep="")
      print(messaggio)
   } else {
      number<-1
      # per ogni fiume -> prendi la riga di listaFiumi e usa url e crea mail alert x livelli e report...
      stampaRecT<-paste("ALERT IDROMETRI!","\n")
      stampaRecTAProb<-paste("ALERT IDROMETRO OFFLINE!","\n")
      stampaRecTASopra<-paste("ALERT LIVELLO SUPERATO!","\n")
      stampaRecTASotto<-paste("ALERT LIVELLO ABBASSATO!","\n")
      for (number in conteggio) {
         fiume<-as.character(tabellaFiumi[number,2])
         idrometro<-as.character(tabellaFiumi[number,3])
         livelloMin<-as.character(tabellaFiumi[number,4])
         livelloMin<-as.numeric(gsub(",",".",livelloMin))
         livelloMax<-as.character(tabellaFiumi[number,5])
         livelloMax<-as.numeric(gsub(",",".",livelloMax))
         aggiornamento<-as.numeric(tabellaFiumi[number,6])
         printTabFiumi<-filter(tabellaIdro2,tabellaIdro2$Luogo==idrometro)
         livelloFiumeUt<-as.character(printTabFiumi$Adesso)
         livelloFiumeUt<-as.numeric(gsub(",",".",livelloFiumeUt))
         livelloFiumeUt<-as.numeric(livelloFiumeUt)
         for (nomeLuogo in luoghi) {
            if (nomeLuogo==idrometro) {
               if (is.na(livelloFiumeUt)) {
                  livelloFiumeUt<-NA
                  if (is.na(livelloFiumeUt)) {
                     stampaRec<-paste("Il fiume", fiume,"a",idrometro,"presenta un problema. Livello",livelloFiumeUt,".\n")
                     stampaRecTAProb<-paste(stampaRecTAProb,stampaRec,sep="")
                     stampaRecTAProb<-paste(stampaRecT,stampaRecTAProb,sep="")
                     print(stampaRecTAProb)
                  }
               }  
               else if (livelloFiumeUt>livelloMin) {
                  stampaRec<-paste("Il fiume", fiume,"a",idrometro,"ha superato il livello impostato di",livelloMin,"metri. Livello:",livelloFiumeUt,"metri.\n")
                  stampaRecTASopra<-paste(stampaRecTASopra,stampaRec,sep="")
                  #print(stampaRecTASopra)
               } 
               else if (livelloFiumeUt<livelloMax) {
                  stampaRec<-paste("Il fiume", fiume,"a",idrometro,"è sceso rispetto al livello impostato di",livelloMax,"metri. Livello:",livelloFiumeUt,"metri.\n")
                  stampaRecTASotto<-paste(stampaRecTASotto,stampaRec,sep="")
                  #print(stampaRecTASotto)
               }
            }
         }
      }
      stampaRecT<-paste(stampaRecT,stampaRecTASopra,stampaRecTASotto,sep="")
      print(stampaRecT)
   } 
   #######################################################
   ###                  INVIO MAIL                     ###
   #######################################################
   
   ### MAIL ALERT ###
   if (grepl("Livello", stampaRecT)==TRUE) {
      body<-stampaRecT[1]
      # from<-"<alert.idrometri@gmx.com>"
      # to<-c("<melloni6@gmail.com>")
      smtp=list(host.name="smtp.gmail.com",port=465,user.name=userlogin,passwd=password,ssl=TRUE)
      # smtp=list(host.name="mail.gmx.com",port=25,user.name="alert.idrometri@gmx.com",passwd="idroalert77")
      authenticate = TRUE
      # send = FALSE
      email<-send.mail(from=from,to=to,subject=subject,html=FALSE,body=body,smtp=smtp,authenticate=T,send=F)
      email$send()
      numeroAlert<-numeroAlert+1
      numeroAlert<-gsub("\\.",",",numeroAlert)
      listaProfile[1,5]
      listaProfile[1,5]<-numeroAlert
   }
   
   elencoFiumi<-c(as.character(tabellaFiumi[1,2]))
   elencoIdro<-c(as.character(tabellaFiumi[1,3]))
   conteggio<-c(2:length(tabellaFiumi$Luogo))
   for (number in conteggio) {
      elencoFiumi<-c(elencoFiumi,c(as.character(tabellaFiumi[number,2])))
      elencoIdro<-c(elencoIdro,c(as.character(tabellaFiumi[number,3])))
      
   }
   tabellaIdro2$Adesso<-as.character(gsub("\\.",",",tabellaIdro2$Adesso))
   tabellaIdro2UT<-data.frame(tabellaIdro2[tabellaIdro2$Luogo %in% elencoIdro,])
   tabellaIdroUt<-merge(tabellaIdro2UT,tabellaFiumi,by="Luogo")
   tabellaIdroUt<-tabellaIdroUt[,c(3,1,7,13,6,12,8,9)]
   # Raggruppo per fiume e regione
   tabellaIdroUt<-group_by(tabellaIdroUt,Fiume.x)
   tabellaIdroUt<-arrange(tabellaIdroUt,Fiume.x,Regione)
   # decommentare e/o modificare il separatore della data
   # tabellaIdro2UT$Data<-gsub("-","/",tabellaIdro2UT$Data)
   nomeFileTabUt<-paste("./report/tabellaFiumi-",nomeUtente,".csv",sep="")
   colnames(tabellaIdroUt)<-c("Fiume","Idrometro","Regione","ASotto","Adesso","ASopra","Data","Ora")
   #install.packages("stargazer")
   #library(stargazer)
   #stargazer(tabellaIdroUt,type="html", title="Descriptive statistics",out="table1.txt")
   #out<-capture.output(View(tabellaIdroUt))
   #cat("Report Idrometri",out,file=nomeFileTabUt,sep="n",append=TRUE)
   
   write.table(tabellaIdroUt,file=nomeFileTabUt,row.names=FALSE,sep=",")
   #source(tabellaIdroUt)
   #pdf(nomeFileTabUt,height=8,width=7)
   #pdf<-print(xtable(tabellaIdroUt[,]),hline.after=c(2,3))
   # grid.table(pdf)
   #dev.off()
   #pdf(write)
   #postscript(file=nomeFileTabUt)
   #pdf
   #dev.off()
   #dat <- pdf[,1:8]
   
   #write.table(tabellaIdroUt,file=nomeFileTabUt,row.names = FALSE,quote=TRUE,sep=",")
   
   ### MAIL REPORT ###
   if (reportUt==1) {
      # from<-"<alert.idrometri@gmx.com>"
      # to<-c("<melloni6@gmail.com>")
      body <-"REPORT LIVELLI TOTALE\nQuesto report è stato generato in automatico.\nPer comunicazioni scrivere a melloni6@gmail.com"
      # smtp=list(host.name = "mail.gmx.com",port=25,user.name="alert.idrometri@gmx.com",passwd="idroalert77")
      smtp=list(host.name="smtp.gmail.com",port=465,user.name=userlogin,passwd=password,ssl=TRUE)
      authenticate = TRUE
      # send = FALSE
      email<-send.mail(from=from,to=to,subject=subject,body=body,smtp=smtp,authenticate=T,send=F,attach.files =nomeFileTabUt)
      email$send()
      numeroReport<-numeroReport+1
      listaProfile[1,6]<-numeroReport
      # sendmail_options(smtpServer="ASPMX.L.GOOGLE.COM")
      # sendmail_options(smtpServer="smtp.gmail.com")
      # sendmail(from,to,"Report idrometri",body)
   }
   write.table(listaProfile,file=nomeFileUtente,row.names=FALSE,sep="|")
}


