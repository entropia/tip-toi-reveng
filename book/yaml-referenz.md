# YAML-Referenz

In diesem Kapitel erfährst du alles über die `.yaml`-Datei, mit der du den
Tiptoi-Stift programmierst.

### YAML-Format: Eine Übersicht

(Generelle Infos zu Yaml.)

### Yaml-Datei-Felder

In der Eingabedatei legt man zuerst ein paar allgemeine Einstellungen fest. Die einzig zwingend notwendige ist die ```product-id```. Hier wird der Code des Einschaltknopfes festgelegt, den jedes Tiptoi-Produkt hat. Hier sollte man eine Nummer nehmen, die keinem offiziellen Produkt entspricht. Erlaubt sind alle Zahlen von 1 bis 999.

Wenn man eine bestehende GME-Datei als Ausgangspunkt genommen hat, sollte man sicherstellen, diese nicht auch noch auf dem Speicher des Stifts liegen zu haben. Ansonsten hat man zwei Dateien für das gleiche Produkt (die gleiche ```product-id```, s.o.) vorliegen. Welche der Stift dann nimmt, ist dann Glückssache… Man kann auch bei der nicht zu verwendenden Datei die Dateiendung ```.gme``` auf irgendetwas anderes ändern, z.B. ```.gmex```.

Eine typischer Yaml-Datei für die Tiptoi-Programmierung sieht so aus:

```yaml
product-id: 42
welcome: hallo
scripts:
  8066:
  - P(erstes_feld)
  8067:
  - P(zweites_feld)
```
Wobei nur die Angabe der ```product-id``` eine Pflichtangabe für gültige Yaml-Datei ist. Die eigentliche Logik, also die Programmierung, landet dann in dem Abschnitt ```scripts```. Hier gibt man - mit Einrückung - die Codes des Tiptoi-Produktes an und zu jedem Code, mit Spiegelstrichen aufgelistet, die auszuführenden Skripte ([Siehe Yaml-Skripte: Register, Bedingungen und Befehle](yaml-referenz.md#yaml-skripte-register-bedingungen-und-befehle))

Es können hier im Kopf aber noch weitere Angaben gemacht und Felder gesetzt werden:

```yaml
product-id: 42
welcome: willkommen
media-path: Audio/
language: de
comment: Ein kurzer Kommentar
init: $modus:=10
scripts:
(...)
```

#### product-id

Die Produkt-ID ist immer eine Ganzzahl, die auf dem Stift einmalig sein sollte. Gerade wenn Du deine GME-Datei anderen gibst, sollte man darauf achten, keine der von Ravensburger vergebenen IDs zu benutzen (siehe ANHANG ?)

#### welcome

Damit man mitbekommt, dass der Stift das eigene Produkt auch erkannt hat, kann man im Feld welcome einen oder - durch Kommas getrennt - mehrere Dateinamen von Audio-Dateien angeben. Diese Dateien sollten im gleichen Verzeichnis wie die YAML-Datei liegen, und im OGG- oder MP3-Format sein. In der YAML-Datei lässt man die Dateiendung allerdings weg.



#### media-path



#### language

#### comment

#### init

#### scripts



### Yaml-Skripte: Register, Bedingungen und Befehle


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
 
Der Wert in einem Register ist immer eine Ganzzahl, lädt man in das Register eine Fließkommazahl, wird diese zu einer abgerundeten Ganzzahl.

```yaml  
- $register:=9 $register/=2 # $register wird zu 4  
```

Ein Register startet immer mit dem Wert 0, außer Du hast oben in der Yaml-Datei dem Register mit "init:" einen anderen Startwert zugewiesen (siehe [HIER LINK NACH OBEN]) h

### "script:", OID-Abschnitt und Befehlszeilen

#### Die Abschnitte 

Der Abschnitt "script:" beinhaltet Unterabschnitte, die jeder für sich einen bestimmten OID-Code repräsentieren. Dort steht, was passieren soll, wenn der tiptoi-Benutzer einen Code antippt. Die Unterabschnitte müssen in deiner Yaml-Datei eingerückt sein.

```yaml
script: # Hier beginnt der script-Abschnitt 
  
  5000: # Hier beginnt der Abschnitt für den OID-Code 5000
  - P(sound1) # Wird der OID-Code mit der Nummer 5000 angetippt, wird die Datei sound1 abgespielt
  
  5010: # Hier beginnt der Abschnitt für den OID-Code 5010
  - P(sound2) # Wird der OID-Code mit der Nummer 5010 angetippt, wird die Datei sound2 abgespielt
```

Siehe auch P().

Alternativ kannst Du statt der Zahlen auch Worte benutzen

```yaml
script: # Hier beginnt der script-Abschnitt 
  
  SoundAbspielen1: # Die OID für diesen Abschnitt wird vom tttool vergeben
  - P(sound1) 
  
  SoundAbspielen2: # Die OID für diesen Abschnitt wird vom tttool vergeben
  - P(sound2) 
```

Hier werden die OIDs von tttool selber vergeben. Du kannst dir die OIDs mit dem Consolenbefehl 'oid-codes' erzeugen lassen. [Siehe Die ttt-Befehle](tttool-referenz)

Mischen kannst Du diese beiden Varianten allerdings nicht.

```yaml
script: 
  
  SoundAbspielen1: 
  - P(sound1) 
  
  5010: 
  - P(sound2)
  ```

Führt zu einem Fehler und es wird keine GME-Datei erzeugt.

#### Die Befehlszeilen



