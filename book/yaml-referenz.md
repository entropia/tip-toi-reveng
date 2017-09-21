# YAML-Referenz
=============

In diesem Kapitel erfährst du alles über die `.yaml`-Datei, mit der du den
Tiptoi-Stift programmierst.

YAML-Format: Eine Übersicht
---------------------------

(Generelle Infos zu Yaml.)

Yaml-Datei-Felder
------------------

(lang, comment, product-id etc.)

Yaml-Skripte: Register, Bedingungen und Befehle
-------------------------------------

### Register

Register werden Variablen genannt, in die man im Programmverlauf Werte ablegen kann. 

Ein Register beginnt immer mit einem $ (Dollarzeichen), gefolgt von mindestens einem und höchstens XXX Zeichen. Dabei ist zu beachten, dass nach dem $ immer zuerst ein Buchstabe kommen muss. Danach können die Zeichen A-Z, a-z, 0-9 und _ benutzt werden. 
 
 Beispiele:
 
```yaml
    - $register:=1 # RICHTIG  
    - $Bla_Bla:=1 # RICHTIG 
    - $Bla-Bla:=1 # FALSCH  (???)
    - $BlaBla7:=1 # RICHTIG 
    - $7BlaBla:=1 # FALSCH  
    - $Bla&Bla:=1 # FALSCH  
``` 

(VERWENDUNG VON $1 usw. FOLGT)
 
Ein Register beinhaltet immer eine Ganzzahl, lädt man in das Register eine Fließkommazahl, wird diese zu einer abgerundeten Ganzzahl.

`  - $register:=9 $register/=2 # $register wird zu 4  `

Ein Register startet immer mit dem Wert 0, außer Du hast oben in der Yaml-Datei dem Register mit "init:" einen anderen Startwert zugewiesen (siehe [HIER LINK NACH OBEN]) h

### "script:", OID-Abschnitt und Befehlszeilen


#### Die Abschnitte 

Der Abschnitt "script:" beinhaltet Unterabschnitte, die jeder für sich einen bestimmten OID-Code repräsentieren. Dort steht, was passieren soll, wenn der tiptoi-Benutzer einen Code antippt. Die Unterabschnitte müssen in deiner Yaml-Datei eingerückt sein.

`
script: # Hier beginnt der script-Abschnitt 
  
  5000: # Hier beginnt der Abschnitt für den OID-Code 5000
  - P(sound1) # Wird der OID-Code mit der Nummer 5000 angetippt, wird die Datei sound1 abgespielt
  
  5010: # Hier beginnt der Abschnitt für den OID-Code 5010
  - P(sound2) # Wird der OID-Code mit der Nummer 5010 angetippt, wird die Datei sound2 abgespielt
  
`
Siehe auch P().

Alternativ kannst Du statt der Zahlen auch Worte benutzen

`
script: # Hier beginnt der script-Abschnitt 
  
  SoundAbspielen1: # Die OID für diesen Abschnitt wird vom tttool vergeben
  - P(sound1) 
  
  SoundAbspielen2: # Die OID für diesen Abschnitt wird vom tttool vergeben
  - P(sound2) 
`

Hier werden die OIDs von tttool selber vergeben. Du kannst dir die OIDs mit dem Consolenbefehl 'oid-codes' erzeugen lassen. [Siehe Die ttt-Befehle](tttool-referenz)

Mischen kannst Du diese beiden Varianten allerdings nicht.

`script: 
  
  SoundAbspielen1: 
  - P(sound1) 
  
  5010: 
  - P(sound2) `

Führt zu einem Fehler und es wird keine GME-Datei erzeugt.

#### Die Befehlszeilen








https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#headers


