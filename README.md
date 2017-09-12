# IDRO ALERT 

Due script R:

1- idroAlertAggiornamentiX.Y.R -> partendo dal file DB con i parametri dei fiumi salva un file con il livello , la differenza con la misura precedente, lo storico (data e ora) del livello minimo e massimo di OGNI IDROMETRO presente.

2- idroAlertProfileX.Y.R -> si occupa di inviare mail di alert sulla base dei parametri impostati per ogni utente. Alert personalizzabili.

USO:

$Rscript idroAlertAggiornamentiX.Y.R
[1] "Tutto è andato bene."
[1] "Ho salvato il file tabellaIdroRep.csv con i valori aggiornati di tutti gli idrometri."
$Rscript idroAlertProfileX.Y.R
[1] invio mail......
$

