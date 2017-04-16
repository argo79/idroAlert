Script in R che permette di configurare un profilo utente (./utente/*.conf) per ottenere una mail di ALERT quando il livello di un idrometro supera un certo livello (ASopra) o quando scende oltre un altro livello (ASotto).

Uno script viene eseguito per ottenere tutti i valori degli idrometri disponibili nel file ./conf/tabellaIdroDB.csv

Un altro script legge ogni profilo utente e se gli alert per un idrometro sono verificati invia un report.

Se è impostato a 1 il parametro "Report" allora invia anche una tabella di tutti i valori degli idrometri scelti.



Cartelle:
  
  ./conf/utenti => file paramtetri utenti.
  
  ./conf/tabellaIdroDB.csv => file parametri fiumi (Nome, Luogo, Url...).
  
  ./report/fiumi => file aggiornato per regione di ogni fiume (YYYY-MM-DD;HH:MM:SS;x,y).
  
  ./report/tabellaIdroRep.csv => tabella aggiornata di tutti i fiumi.
  
  ./idroAlertAggiornamento2.1.R => script di aggiornamento di tutti i fiumi, salva in ./report/tabellaIdroRep.csv
  
  ./idroAlertReport2.3.R => script di controllo alert e invio mail di alert e report (allega tabella utente)
  
  Impostare crontab per esecuzione dei due script. 
  
  Minimo30 minuti per aggiornamento e 1 ora per il report.
  
