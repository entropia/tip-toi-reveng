YAML-Referenz
=============

In diesem Kapitel erfährst du alle Details zur YAML-Datei, mit der du den Stift
programmierst.

YAML-Format: Eine Übersicht
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Das YAML-Format im allgemeinen wurde nicht für dieses Projekt erfunden, sondern ist allgemein gebräuchlich um strukturierte Daten in einer menschenlesbaren Textdatei abzulegen. Eine knappe Übersicht findest du auf der `Wikipedia-Seite zu YAML <https://de.wikipedia.org/wiki/YAML>`_.
Beachte dass in YAML Einrückungen, also Leerzeichen am Anfang der Zeile, relevant sind und die Struktur der Datei beschreiben sind!

Die YAML-Datei bearbeitest du mit dem Texteditor deiner Wahl (Notepad, vim etc.).


YAML-Datei-Felder
~~~~~~~~~~~~~~~~~

Eine typische, minimale YAML-Datei für die Tiptoi-Programmierung sieht so aus:

.. code:: yaml

    product-id: 950
    scripts:
      8066:
      - P(erstes_feld)
      8067:
      - P(zweites_feld)


Zwingend nötig ist dabei streng genommen nur das Feld ``product-id``, aber in der Regel brauchst du auch ``scripts`` um irgend etwas Sinnvolles zu machen.

Es gibt noch eine Reihe weiterer, optionaler Felder, die im Folgenden erklärt werden. Eine etwas kompliziterte Datei könnte dann zum Beispiel so aussehen:

.. code:: yaml

    product-id: 950
    comment: Ein kurzer Kommentar
    welcome: willkommen
    media-path: Audio/%s
    gme-lang: GERMAN
    init: $modus:=10
    scripts:
       haus:
       - P(hello)
       tuer:
       - P(goodbye)
       monster:
       - P(warning)
    language: en
    speak:
       hello: "Hello, friend!"
       goodbye: "Goodbye"
       warning: "Watch out."
    scriptcodes:
       haus: 4718
       tuer: 4716
       warning: 4717


``product-id``
^^^^^^^^^^^^^^
Format:
  Ein OID-Code im Bereich 0 bis 999

Beispiel:
  .. code:: yaml

    product-id: 950

Zweck:
  Die Produkt-ID dieses Projekts. Das Anschaltfeld des Produktes sollte mit dem
  hier angegebenen OID-Code bedruckt sein.

Es sollte zu jeder Produkt-ID nur eine GME-Datei auf den Stift geladen werden. Ravensburger zählt seine Produkte fortlaufend ab 1
hoch, und Sprachen werden mit 999 abwärts nummeriert. Wir raten dir für dein eigenes Projekt daher eine Zahl zwischen 900 und 950.

``comment``
^^^^^^^^^^^

Format:
  Ein Textstring

Beispiel:
  .. code:: yaml

    comment: "Mein Tiptoi-Produkt"

Zweck:
  Der Kommentar wird in der GME-Datei gespeichert, aber sonst ignoriert.


``welcome``
^^^^^^^^^^^

Format:
  Einen oder mehrere Audio-Dateinamen, durch Kommata getrennt

Beispiel:
  .. code:: yaml

    welcome: hello

Zweck:
  Beim Aktivieren des Produktes werden die angegebenen Audio-Dateien abgespielt.

``media-path``
^^^^^^^^^^^^^^

Format:
  Ein Dateipfad, mit ``%s`` als Platzhalter

Beispiel:
  .. code:: yaml

    media-path: Audio/%s

Zweck:
  Gibt an, wo sich die Audiodateien befinden. Der Platzhalter ``%s`` wird dabei
  durch den in der YAML-Datei verwendeten Dateinamen ersetzt. Das Programm
  sucht nach allen geeigneten Dateiendungen (``.wav``, ``.ogg``, ``.flac``, ``.mp3``).

  Beispiel: Den im ``welcome: hello`` angegebenen Begrüßungssound würde das
  ``tttool`` also die Datei ``Audio/hello.ogg`` einbinden.

``gme-lang``
^^^^^^^^^^^^

Format:
  Eine Sprache (``GERMAN``, ``ENGLISH``, ``FRENCH``\…)

Beispiel:
  .. code:: yaml

    gme-lang: GERMAN

Zweck:
  Das Sprach-Feld der GME-Datei. Bei eigenen Produkten gibt es in der Regel
  kein Grund, dieses Feld anzugeben.

``init``
^^^^^^^^

In diesem Feld werden Strings initialisiert. Beispielsweise werden hier
Spielmodi oder Zähler auf 0 gesetzt, damit diese Später mit einem
vorgegebenen Wert starten können. Beispiel: Wird hier "$modus:=0 $i:=0"
geschrieben, so wird nach Aktivierung des Projekts der String modus und
i mit 0 gestartet.

``scripts``
^^^^^^^^^^^

Dies ist der eigentliche Hauptarbeitsbereich für unsere Projekte. In
diesem Feld werden unsere Programmierungen geschrieben.


``language``
^^^^^^^^^^^^

Format:
  Ein Sprach-Kürzel (``de``, ``en``, ``fr``\ …)

Beispiel:
  .. code:: yaml

    language: de

Zweck:
  Gibt die Sprache für die Sprachsynthese (siehe Feld ``speak``) an.

``speak``
^^^^^^^^

Das tttool verfügt über ein integriertes Text-2-Speech tool, welches es
erlaubt Texte in Form von Audiofiles zu integrieren. Dies dient uns
Beispielsweise während der Entwicklung dazu Texte nicht immer aufnehmen
und einspeichern zu müssen. Beispiel: hallo\_welt: "Hallo Welt! Ich
hoffe dir geht es gut." Im Beispiel wird nun automatisiert ein Audiofile
erstellt, welches mit "P(hallo\_welt)" abgespielt werden kann.

``scriptcodes``
^^^^^^^^^^^^^^^

YAML-Skripte: Register, Bedingungen und Befehle
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(allgemeiner text)

Register
~~~~~~~~

Register werden Variablen genannt, in die man im Programmverlauf Werte
ablegen kann.

Ein Register beginnt immer mit einem $ (Dollarzeichen), gefolgt von
mindestens einem und höchstens XXX Zeichen. Dabei ist zu beachten, dass
nach dem $ immer zuerst ein Buchstabe kommen muss. Danach können die
Zeichen A-Z, a-z, 0-9 und \_ benutzt werden.

Beispiele:

.. code:: yaml

        - $register:=1 # RICHTIG  
        - $Bla_Bla:=1 # RICHTIG 
        - $Bla-Bla:=1 # FALSCH  (???)
        - $BlaBla7:=1 # RICHTIG 
        - $7BlaBla:=1 # FALSCH  
        - $Bla&Bla:=1 # FALSCH  

(VERWENDUNG VON $1 usw. FOLGT)

Der Wert in einem Register ist immer eine Ganzzahl, lädt man in das
Register eine Fließkommazahl, wird diese zu einer abgerundeten Ganzzahl.

.. code:: yaml

    - $register:=9 $register/=2 # $register wird zu 4  

Ein Register startet immer mit dem Wert 0, außer Du hast oben in der
YAML-Datei dem Register mit "init:" einen anderen Startwert zugewiesen
(siehe [HIER LINK NACH OBEN]) h

"script:", OID-Abschnitt und Befehlszeilen
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Die Abschnitte
^^^^^^^^^^^^^^

Der Abschnitt "script:" beinhaltet Unterabschnitte, die jeder für sich
einen bestimmten OID-Code repräsentieren. Dort steht, was passieren
soll, wenn der tiptoi-Benutzer einen Code antippt. Die Unterabschnitte
müssen in deiner YAML-Datei eingerückt sein.

.. code:: yaml

    script: # Hier beginnt der script-Abschnitt 
      
      5000: # Hier beginnt der Abschnitt für den OID-Code 5000
      - P(sound1) # Wird der OID-Code mit der Nummer 5000 angetippt, wird die Datei sound1 abgespielt
      
      5010: # Hier beginnt der Abschnitt für den OID-Code 5010
      - P(sound2) # Wird der OID-Code mit der Nummer 5010 angetippt, wird die Datei sound2 abgespielt

Siehe auch P().

Alternativ kannst Du statt der Zahlen auch Worte benutzen

.. code:: yaml

    script: # Hier beginnt der script-Abschnitt 
      
      SoundAbspielen1: # Die OID für diesen Abschnitt wird vom tttool vergeben
      - P(sound1) 
      
      SoundAbspielen2: # Die OID für diesen Abschnitt wird vom tttool vergeben
      - P(sound2) 

Hier werden die OIDs von tttool selber vergeben. Du kannst dir die OIDs
mit dem Consolenbefehl 'oid-codes' erzeugen lassen. `Siehe Die
ttt-Befehle <tttool-referenz>`__

Mischen kannst Du diese beiden Varianten allerdings nicht.

\`\`\`yaml script:

SoundAbspielen1: - P(sound1)

5010: - P(sound2) \`\`\`

Führt zu einem Fehler und es wird keine GME-Datei erzeugt.

Die Befehlszeilen
~~~~~~~~~~~~~~~~~

-  trennung der anweisungen
-  anzahl befehle/zeile

Bedingte Anweisung
~~~~~~~~~~~~~~~~~~

-  mehrere innerhalb einer Zeile, was wird ausgeführt
-  einer pro Zeile, Mehrzeilen, was wird ausgeführt
-  siehe Play
-  siehe jump
-  siehe $modus
-  siehe schleifen

Befehle
~~~~~~~

XXXX Befehle (XXXX ... P J usw.)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(EINLEITENDER TEXT)

P()
^^^

-  einzel play
-  random play
-  besonderheit play und jump

P\*()
^^^^^

(TEXT FEHLT NOCH)

PA\*()
^^^^^^

(TEXT FEHLT NOCH)

PA\*()
^^^^^^

(TEXT FEHLT NOCH)

PA()
^^^^

(TEXT FEHLT NOCH)

J()
^^^

(TEXT FEHLT NOCH)

G()
^^^

(TEXT FEHLT NOCH)

C
^

(TEXT FEHLT NOCH)

T()
^^^

(TEXT FEHLT NOCH)

?() ()
^^^^^^

(TEXT FEHLT NOCH)

Registerbefehle
~~~~~~~~~~~~~~~

Mit Registerbefehlen lassen sich Werte in einem Register setzen oder
ändern. Registerbefehle sind in der Regel so Aufgebaut:
(Register)(Anweisung)(Wert). Also zum Beispiel:

.. code:: yaml

      - $modus+=5 

In diesem Beispiel wird der Registerbefehl Addition verwendet. Das
bedeutet, dass zu dem augenblicklichen Wert von $modus, 5 addiert wird.

:= (Register setzen)
^^^^^^^^^^^^^^^^^^^^

Der Befehl ``:=`` setzt das Register auf den Wert hinter dem
Gleichheitszeichen

.. code:: yaml

     - $r:=5 # Hier wird das Register $r auf den Wert 5 gesetzt

+= -= \*= /= (Grundrechenarten)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: yaml

    - $r+=5 # Hier wird zum Registerwert $r 5 addiert

.. code:: yaml

    - $r-=5 # Hier wird vom Registerwert $r 5 subtrahiert

.. code:: yaml

    - $r*=5 # Hier wird der Wert vom Register $r mit 5 multipliziert

.. code:: yaml

     - $r%=5 # Hier wird der Wert vom Register $r durch 5 geteilt und abgerundet

Der Befehl „/=“ teilt den Wert vom Register $r durch die Zahl hinter dem
Gleichheitszeichen. Dabei ist zu beachten, dass immer ein Integer
(Ganzzahl) geliefert und das Ergebnis abgerundet wird. Die Rechnung von
9 durch 2 ergibt also 4.

%= (Register modulo)
^^^^^^^^^^^^^^^^^^^^

Der Befehl „%=“ liefert das modulo des Registers mit der Zahl hinter dem
Gleichheitszeichen

.. code:: yaml

    - $r%=5 # Hier wird das Modulo (teiler Rest) von $r modulo 5 geliefert 

Angenommen $r hat einen Wert von 23 und man Teil das durch 5, dann Wäre
das Ergebnis 4 Rest 3. In dem Beispielen oben hätte $r nach dem
Registerbefehl 3.

Neg() (Register Negieren)
^^^^^^^^^^^^^^^^^^^^^^^^^

Der Befehl „Neg()“ negiert den Wert eines Registers. Hat das Register
zum Beispiel den Wert 5, wird nach dem Befehl der Wert -5. Aus -5 würde
5 werden. Dieser Registerbefehl wird anders als die Anderen mit klammern
geschrieben.

.. code:: yaml

    - Neg($r) # Hier wird der Wert des Registers $r negiert.

Bitweise Operatoren
^^^^^^^^^^^^^^^^^^^

Wenn Du nicht weißt was Bitweise UND, OR und XOR ist, dann wirst Du
diese Befehle wahrscheinlich nicht brauchen. Um Bitweise Operatoren zu
verstehen muss man wissen wie eine Dezimalzahl in Binärschreibweise
Dargestellt wird. (`siehe
Wikipedia <https://de.wikipedia.org/wiki/Dualsystem>`__)

&= (Bitweise UND)
^^^^^^^^^^^^^^^^^

Der Befehl „&=“ wendet den Wert hinter dem Gleichheitszeichen auf das
Register an. Ein bitweises UND wird auf zwei Bitfolgen gleicher Länge
angewendet und führt die logische UND-Verknüpfung auf jedem Paar
korrespondierender Bits durch. Das Ergebnisbit ist 1, falls beide Bits 1
sind, ansonsten ist es 0.

.. code:: yaml

    - $r&=5 # Hier wird 5 Bitweise UND auf das Register $r angewendet

Bitweise OR
^^^^^^^^^^^

XXX FOLGT XXX

FFF6 (written $r\|=m): bitwise or to register $r the value of m

Bitweise XOR
^^^^^^^^^^^^

XXX FOLGT XXX

FFF7 (written $r^=m): bitwise xor to register $r the value of m

YAML-Skripte: Bedingungen und Befehle
-------------------------------------
