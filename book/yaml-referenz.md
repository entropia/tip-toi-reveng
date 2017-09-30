# YAML-Referenz

In diesem Kapitel erfährst du alles, oder zumindest vieles über die `.yaml`-Datei, mit der du den Tiptoi-Stift programmierst. 

### YAML-Format: Eine Übersicht

(HIER Generelle Infos zu Yaml.)

### Yaml-Datei-Felder

In der Eingabedatei legt man zuerst ein paar allgemeine Einstellungen fest. Die einzig zwingend notwendige ist die ```product-id```. Hier wird der Code des Einschaltknopfes festgelegt, den jedes Tiptoi-Produkt hat. Erlaubt sind alle Zahlen von 1 bis 999. Hier sollte man eine Nummer nehmen, die keinem offiziellen Produkt entspricht. Nach dem Einschalten des Stifts werden die ```product-ids``` aller GME-Datein auf dem Stift eingelesen und es kann dabei zu konflikten kommen.

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
Wobei nur die Angabe der ```product-id``` eine Pflichtangabe für gültige Yaml-Datei ist. Die eigentliche Logik, also die Programmierung, landet dann in dem Abschnitt ```scripts```. Hier gibt man - mit Einrückung - die Codes des Tiptoi-Produktes an und zu jedem Code, mit Spiegelstrichen aufgelistet, die auszuführenden Skripte ([Siehe script, OID-Abschnitt und Befehlszeilen](yaml-referenz.md#script-oid-abschnitt-und-befehlszeilen))

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

Der hier angegebene OID-Code startet das Projekt. Sollte von Ravensburger die Selbe ID genutzt werden, wird es auf dem Stift beim Starten des Projekts zu einem Konflikt kommen. Daher ist diese ID so zu wählen, dass Sie einmalig ist. Da Ravensburger fortlaufend ab 1 hochzählt, wird eine Zahl von 900 bis höher empfohlen. Somit bleibt uns noch eine lange Zeit, bis es zu einem Konflikt kommen könnte. Da 999 abwärts für Sprachen genutzt wird, rate ich zu einer ID von 900-950.

#### welcome

Damit man mitbekommt, dass der Stift das eigene Produkt auch erkannt hat, kann man im Feld welcome einen oder - durch Kommas getrennt - mehrere Dateinamen von Audio-Dateien angeben. Diese Dateien sollten im gleichen Verzeichnis wie die YAML-Datei liegen, und im OGG- oder MP3-Format sein. In der YAML-Datei lässt man die Dateiendung allerdings weg.


#### media-path

Im diesem Feld wird vorgegeben, wo sich die Audiofiles zum Programm befinden. Der Ordner wird hier inkremental angegeben. Wird hier beispielsweise "media/test_%s" geschrieben, so wird der String "%s" durch den Namen im Programm ersetzt.
Beispiel: Ein Aufruf mit P(audio) greift auf die Datei media/test_audio.ogg zurück.


#### language

Wie der Name hier bereits vermuten lässt, lässt sich hier die Sprache des TipToi-Stiftes angeben. Für Deutsch wählen wir "de".

#### comment

Dies ist das Kommentarfeld für uns Programmierer. Kommentiere dein Projekt nach Lust und Laune.

#### init

In diesem Feld werden Strings initialisiert. Beispielsweise werden hier Spielmodi oder Zähler auf 0 gesetzt, damit diese Später mit einem vorgegebenen Wert starten können. 
Beispiel: Wird hier "$modus:=0 $i:=0" geschrieben, so wird nach Aktivierung des Projekts der String modus und i mit 0 gestartet.

#### scripts

Dies ist der eigentliche Hauptarbeitsbereich für unsere Projekte. In diesem Feld werden unsere Programmierungen geschrieben.

#### speak

Das tttool verfügt über ein integriertes Text-2-Speech tool, welches es erlaubt Texte in Form von Audiofiles zu integrieren. Dies dient uns Beispielsweise während der Entwicklung dazu Texte nicht immer aufnehmen und einspeichern zu müssen.
Beispiel: hallo_welt: "Hallo Welt! Ich hoffe dir geht es gut."
Im Beispiel wird nun automatisiert ein Audiofile erstellt, welches mit "P(hallo_welt)" abgespielt werden kann.

### Yaml-Skripte: Register, Bedingungen und Befehle

(allgemeiner text)

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

### Die Befehlszeilen

- trennung der anweisungen
- anzahl befehle/zeile


### Bedingte Anweisung

- mehrere innerhalb einer Zeile, was wird ausgeführt
- einer pro Zeile, Mehrzeilen, was wird ausgeführt
- siehe Play
- siehe jump
- siehe $modus
- siehe schleifen

### Befehle

### XXXX Befehle (XXXX ... P J usw.)

(EINLEITENDER TEXT)

#### P()

- einzel play
- random play
- besonderheit play und jump

#### P*()

(TEXT FEHLT NOCH)

#### PA*()

(TEXT FEHLT NOCH)

#### PA*()

(TEXT FEHLT NOCH)

#### PA()

(TEXT FEHLT NOCH)

#### J()

(TEXT FEHLT NOCH)

#### G()

(TEXT FEHLT NOCH)

#### C

(TEXT FEHLT NOCH)

#### T()

(TEXT FEHLT NOCH)

#### ?() ()

(TEXT FEHLT NOCH)


### Registerbefehle

Mit Registerbefehlen lassen sich Werte in einem Register setzen oder ändern. Registerbefehle sind in der Regel so Aufgebaut: (Register)(Anweisung)(Wert). Also zum Beispiel:

```yaml
  - $modus+=5 
```
In diesem Beispiel wird der Registerbefehl Addition verwendet. Das bedeutet, dass zu dem augenblicklichen Wert von $modus, 5 addiert wird.   

#### := (Register setzen)

Der Befehl ```:=``` setzt das Register auf den Wert hinter dem Gleichheitszeichen

```yaml
 - $r:=5 # Hier wird das Register $r auf den Wert 5 gesetzt
```

#### += -= *= /= (Grundrechenarten)

```yaml
- $r+=5 # Hier wird zum Registerwert $r 5 addiert
```
```yaml
- $r-=5 # Hier wird vom Registerwert $r 5 subtrahiert
```
```yaml
- $r*=5 # Hier wird der Wert vom Register $r mit 5 multipliziert
```
```yaml
 - $r%=5 # Hier wird der Wert vom Register $r durch 5 geteilt und abgerundet
```

Der Befehl „/=“ teilt den Wert vom Register $r durch die Zahl hinter dem Gleichheitszeichen. Dabei ist zu beachten, dass immer ein Integer (Ganzzahl) geliefert und  das Ergebnis abgerundet wird. Die Rechnung von 9 durch 2 ergibt also 4.

#### %= (Register modulo)

Der Befehl „%=“ liefert das modulo des Registers mit der Zahl hinter dem Gleichheitszeichen

```yaml
- $r%=5 # Hier wird das Modulo (teiler Rest) von $r modulo 5 geliefert 
```

Angenommen $r hat einen Wert von 23 und man Teil das durch 5, dann Wäre das Ergebnis 4 Rest 3. In dem Beispielen oben hätte $r nach dem Registerbefehl 3.

#### Neg() (Register Negieren)

Der Befehl „Neg()“ negiert den Wert eines Registers. Hat das Register zum Beispiel den Wert 5, wird nach dem Befehl der Wert -5. Aus -5 würde 5 werden. Dieser Registerbefehl wird anders als die Anderen mit klammern geschrieben.

```yaml
- Neg($r) # Hier wird der Wert des Registers $r negiert.
```

#### Bitweise Operatoren

Wenn Du nicht weißt was Bitweise UND, OR und XOR ist, dann wirst Du diese Befehle wahrscheinlich nicht brauchen. Um Bitweise Operatoren zu verstehen muss man wissen wie eine Dezimalzahl in Binärschreibweise Dargestellt wird. ([siehe Wikipedia](https://de.wikipedia.org/wiki/Dualsystem))

#### &= (Bitweise UND)

Der Befehl „&=“ wendet den Wert hinter dem Gleichheitszeichen auf das Register an. Ein bitweises UND wird auf zwei Bitfolgen gleicher Länge angewendet und führt die logische UND-Verknüpfung auf jedem Paar korrespondierender Bits durch. Das Ergebnisbit ist 1, falls beide Bits 1 sind, ansonsten ist es 0.

```yaml
- $r&=5 # Hier wird 5 Bitweise UND auf das Register $r angewendet
```

#### Bitweise OR

XXX FOLGT XXX

FFF6 (written $r|=m): bitwise or to register $r the value of m

#### Bitweise XOR

XXX FOLGT XXX

FFF7 (written $r^=m): bitwise xor to register $r the value of m


Yaml-Skripte: Bedingungen und Befehle
-------------------------------------
