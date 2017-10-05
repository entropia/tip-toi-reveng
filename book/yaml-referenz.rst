YAML-Referenz
=============

In diesem Kapitel erfährst du alle Details zur YAML-Datei, mit der du den Stift
programmierst.

Das YAML-Format
~~~~~~~~~~~~~~~

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

.. _yaml-init:

``init``
^^^^^^^^

In diesem Feld werden Strings initialisiert. Beispielsweise werden hier
Spielmodi oder Zähler auf 0 gesetzt, damit diese Später mit einem
vorgegebenen Wert starten können. Beispiel: Wird hier "$modus:=0 $i:=0"
geschrieben, so wird nach Aktivierung des Projekts der String modus und
i mit 0 gestartet.

``scripts``
^^^^^^^^^^^

Format:
  Eine Zuordnung von OID-Codes (oder Code-Namen) zu einer Liste von Skriptzeilen.

Beispiel:
  .. code:: yaml

    scripts:
      8067:
       - P(hi)
      haus:
       - $mode==3? P(welcome)
       - P(goodbye)

Zweck:
  Enthält die Logik dieses Tiptoi-Produktes, und gibt für ein OID-Code an, was der Stift machen soll, wenn du diesen Code antippst.

  Statt einem konkreten OID-Code kann auch ein Code-Name angegeben werden, siehe Abschnitt „:ref:`code-namen`“.

  Die Skripte werden in Detail im Abschnitt „:ref:`yaml-skripte`“ erklärt.

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
^^^^^^^^^

Format:
  Eine Zuordnung von Dateinamen zu Text

Beispiel:
  .. code:: yaml

    speak:
       hello: "Hello, friend!"
       goodbye: "Goodbye"
       warning: "Watch out."

Zweck:
  Gibt an, welche Audiodateien das ``tttool`` per Text-to-Speech generieren
  soll, sofern es die entsprechenden Audiodateien nicht findet. Dabei wird die
  in ``language`` angegebene Sprache verwendet.


Das tttool verfügt über ein integriertes Text-to-Speech tool, welches dir
erlaubt Texte automatisch vorgelesen einzubauen. So kannst du deine Tiptoi-Entwicklung testen, bevor du alles nötige aufgenommen hast.

Solltest du Text-to-Speech in verschiedenen Sprachen benötigen, kannst du mehrere Abschnitte mit eigener Sprache angeben:

  .. code:: yaml

    speak:
    - language: en
       hello: "Hello, friend!"
       goodbye: "Goodbye"
    - language: de
       warning: "Achtung!"


.. _code-namen:

``scriptcodes``
^^^^^^^^^^^^^^^

Format:
  Eine Zuordnung von Codenamen zu OID-Code

Beispiel:
  .. code:: yaml

    scriptcodes:
       haus: 4718
       tuer: 4716
       warning: 4717

Zweck:
  Erlaubt dir, im Abschnitt ``scripts`` und in ``J``-Befehlen mit sprechenden Namen
  statt OID-Codes zu arbeiten. Bei der Erstellung der GME-Datei wird in dieser
  Zuordnung nachgeschlagen, welcher OID-Code verwendet werden soll.

Du kannst sprechende Namen auch ohne ``scriptcodes`` verwenden, in diesem Fall
wählt das ``tttool`` die Codes selbst. Damit stets die gleichen Codes verwendet
werden, speichert es die Auswahl in einer Datei mit Endung ``.codes.yaml``, die
nur den ``scriptcodes``-Eintrag enthält. Es steht dir frei, diese Zuordnung in
die eigentliche YAML-Datei zu übernehmen.

.. warning::

   Das ``tttool`` arbeitet *entweder* mit Namen *oder* mit Nummern. Du kannst
   die beiden Varianten nicht mischen.


.. _yaml-skripte:

YAML-Programmierung
~~~~~~~~~~~~~~~~~~~

Die Logik einer Tiptoi-Programmierung steckt vor allem in den im ``scripts``-Feld angegebenen Skripten. Es gibt zu jedem OID-Code ein Skript. Ein Skript besteht aus einer oder gegebenenfalls mehrerer Zeilen, die wiederum aus Befehlen bestehen.

Das einfachste Beispiel ist also

.. code:: yaml

     scripts:
       2000: P(hallo)

Hier wird, wenn du den OID-Code 2000 antippst, der Befehl ``P(hallo)`` ausgeführt. (Die Befehle selbst werden in Kürze erklärt.)

Eine Skriptzeile kann mehrere Befehle enthalten, etwa

.. code:: yaml

     scripts:
       2000: P(hallo) P(freund) J(2001)

Hier werden drei Befehler nacheinander ausgeführt.

.. warning::

  Soweit bekannt kann es zu Problemen kommen, wenn **mehr als 8** Befehle in
  einer Zeile stehen. Darüber hinaus interagieren manche Befehle seltsam; mehr
  dazu im Abschnitt „:ref:`command-J`\ “.

Im Allgemeinen können zu einem Skript mehrere Zeilen angegeben werden:

.. code:: yaml

     scripts:
       2000:
        - $offen==1? P(willkommen)
        - $offen==0? P(finde_den_schluessel)

Tippst du nun Code 2000 an, wird die erste Zeile ausgeführt, deren Bedingungen  alle erfüllt sind (mehr zum Programmieren mit Bedingungen im Abschnitt „:ref:`conditionals`\ “).

Statt die OID-Codes numerisch anzugeben, kannst du auch sprechende Namen verwenden, siehe Abschnitt „:ref:`code-namen`\ “.

Register
^^^^^^^^
Viele Befehle manipulieren :index:`\ <Register>`\ *Register*. Diese
repräsentieren Speicherzellen, in denen im Programmverlauf Werte abgelegt und
abgerufen werden können. Man könnte sie auch Variablen nennen.

Der Name eines Registers beginnt immer mit einem `$`, gefolgt von Buchstaben,
Zahlen oder Unterstrichen (`_`). Direkt nach dem `$` muss ein Buchstabe kommen.

Alle Arithmetik auf dem Tiptoistift arbeitet mit ganzen Zahlen im Bereich 0 bis 65535). Alle Register haben zu Beginn den Wert 0, sofern du es nicht im ``init``-Feld anders verlangst (siehe Abschnitt „:ref:`yaml-init`\ “.

Wenn du eine GME-Datei exportierst (siehe Abschnitt „:ref:`tttool-export`\ “), so kennt das ``tttool`` die Namen der Register nicht. In dem Fall werden Nummern verwendet (``$0``, ``$1``\ …). Es gibt in der Regel keinen Grund, dies in deinen eigenen Tiptoi-Produkten so zu machen. 

Befehlsreferenz
~~~~~~~~~~~~~~~

Im Folgenden werden die Befehle im Einzelnen erklärt: Wie du sie in der YAML-Datei schreibst, was sie bewirken, und was sonst so dabei zu beachten ist.


``P()`` – Audio abspielen
^^^^^^^^^^^^^^^^^^^^^^^^^

Format 1:
  ``P(``\ *audio-datei*\ ``)``
Format 2:
  ``P(``\ *audio-datei*\ ``,``\ *audio-datei*\ ``,``\ …\ ``)``
Beispiel:
  .. code:: yaml

    haus:
    - P(willkommen) P(zu_hause,daheim)

In der ersten Form spielt der Befehl die angegene Audio-Datei ab.

In der zweiten Form spielt der Befehl einen zufälligen der angegebenen Audio-Dateien ab.


``J()`` – Sprung
^^^^^^^^^^^^^^^^

(TEXT FEHLT NOCH)

TODO: Besonderheit play und jump

``T()`` – Zufall
^^^^^^^^^^^^^^^^

(TEXT FEHLT NOCH)


``:=`` – Register setzen
^^^^^^^^^^^^^^^^^^^^^^^^

Der Befehl ``:=`` setzt das Register auf den Wert hinter dem
Gleichheitszeichen

.. code:: yaml

     - $r:=5 # Hier wird das Register $r auf den Wert 5 gesetzt

Mit Registerbefehlen lassen sich Werte in einem Register setzen oder
ändern. Registerbefehle sind in der Regel so Aufgebaut:
(Register)(Anweisung)(Wert). Also zum Beispiel:

.. code:: yaml

      - $modus+=5 

In diesem Beispiel wird der Registerbefehl Addition verwendet. Das
bedeutet, dass zu dem augenblicklichen Wert von $modus, 5 addiert wird.

``+=``, ``-=``, ``*=``, ``/=`` – Grundrechenarten
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

``%=`` – Modulo
^^^^^^^^^^^^^^^

Der Befehl „%=“ liefert das modulo des Registers mit der Zahl hinter dem
Gleichheitszeichen

.. code:: yaml

    - $r%=5 # Hier wird das Modulo (teiler Rest) von $r modulo 5 geliefert 

Angenommen $r hat einen Wert von 23 und man Teil das durch 5, dann Wäre
das Ergebnis 4 Rest 3. In dem Beispielen oben hätte $r nach dem
Registerbefehl 3.

``Neg()`` –  Register negieren
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Der Befehl „Neg()“ negiert den Wert eines Registers. Hat das Register
zum Beispiel den Wert 5, wird nach dem Befehl der Wert -5. Aus -5 würde
5 werden. Dieser Registerbefehl wird anders als die Anderen mit klammern
geschrieben.

.. code:: yaml

    - Neg($r) # Hier wird der Wert des Registers $r negiert.

``&=``, ``|=``, ``^=`` – bitweise Operatoren
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Wenn Du nicht weißt was Bitweise UND, OR und XOR ist, dann wirst Du
diese Befehle wahrscheinlich nicht brauchen. Um Bitweise Operatoren zu
verstehen muss man wissen wie eine Dezimalzahl in Binärschreibweise
Dargestellt wird. (`siehe
Wikipedia <https://de.wikipedia.org/wiki/Dualsystem>`__)


Der Befehl „&=“ wendet den Wert hinter dem Gleichheitszeichen auf das
Register an. Ein bitweises UND wird auf zwei Bitfolgen gleicher Länge
angewendet und führt die logische UND-Verknüpfung auf jedem Paar
korrespondierender Bits durch. Das Ergebnisbit ist 1, falls beide Bits 1
sind, ansonsten ist es 0.

.. code:: yaml

    - $r&=5 # Hier wird 5 Bitweise UND auf das Register $r angewendet

FFF6 (written $r\|=m): bitwise or to register $r the value of m

Bitweise XOR

FFF7 (written $r^=m): bitwise xor to register $r the value of m

Bedingungen
^^^^^^^^^^^

Weitere Befehle
^^^^^^^^^^^^^^^

(Befehle die der normale Tiptoi-Bastler nicht braucht, aber die das ``tttool`` ausspuckt)

P\*()
-----

(TEXT FEHLT NOCH)

PA\*()
------

(TEXT FEHLT NOCH)

PA\*()
------

(TEXT FEHLT NOCH)

PA()
----

(TEXT FEHLT NOCH)

G()
---

(TEXT FEHLT NOCH)

C
-

(TEXT FEHLT NOCH)

?() ()
------

(TEXT FEHLT NOCH)

