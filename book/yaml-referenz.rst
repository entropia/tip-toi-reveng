YAML-Referenz
=============

In diesem Kapitel erfährst du alle Details zur YAML-Datei, mit der du den Stift
programmierst.

Das YAML-Format
~~~~~~~~~~~~~~~

Das YAML-Format im Allgemeinen wurde nicht für dieses Projekt erfunden, sondern ist gebräuchlich, um strukturierte Daten in einer menschenlesbaren Textdatei abzulegen. Eine knappe Übersicht findest du auf der `Wikipedia-Seite zu YAML <https://de.wikipedia.org/wiki/YAML>`_.
Beachte, dass in YAML Einrückungen, also Leerzeichen am Anfang der Zeile, relevant sind und die Struktur der Datei beschreiben!

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


Zwingend nötig ist dabei streng genommen nur das Feld ``product-id``, aber in der Regel brauchst du auch ``scripts`` um irgendetwas Sinnvolles zu machen.

Es gibt noch eine Reihe weiterer, optionaler Felder, die im Folgenden erklärt werden. Eine etwas kompliziertere Datei könnte dann zum Beispiel so aussehen:

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
hoch und Sprachen werden mit 999 abwärts nummeriert. Wir raten dir für dein eigenes Projekt daher eine Zahl zwischen 900 und 950 zu wählen.

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

.. _media-path:

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

  Beispiel: Für den im ``welcome: hello`` angegebenen Begrüßungssound würde das
  ``tttool`` also die Datei ``Audio/hello.ogg`` einbinden.
  
  Beginnt der Dateiname aller Deiner Sounddatein gleich, kannst du diese Prefix im media-path angeben.
  Es werden dann alle Sounds (also auch die bei dem P()-Befehl angegebenen) mit diesem Anfang eingebunden. Wenn sich
   mehrere Deiner tttool-Projekte einen Audio-Ordner teilen, kann das sehr viel übersichtlicher sein.
  
  Beispiel: ``media-path: Audio/schatzsuche_%s`` würde bei einem ``welcome: hello`` die Datei ``Audio/schatzsuche_hello.ogg``
   einbinden.
  

``gme-lang``
^^^^^^^^^^^^

Format:
  Eine Sprache (``GERMAN``, ``ENGLISH``, ``FRENCH``\…)

Beispiel:
  .. code:: yaml

    gme-lang: GERMAN

Zweck:
  Das Sprach-Feld der GME-Datei. Bei eigenen Produkten gibt es in der Regel
  keinen Grund, dieses Feld anzugeben.

.. _yaml-init:

``init``
^^^^^^^^

In diesem Feld werden Strings initialisiert. Beispielsweise werden hier
Spielmodi oder Zähler auf 0 gesetzt, damit diese später mit einem
vorgegebenen Wert starten können. Beispiel: Wird hier "$modus:=0 $i:=0"
geschrieben, so werden nach Aktivierung des Projekts die Strings modus und
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
  Enthält die Logik dieses Tiptoi-Produktes und gibt für einen OID-Code an was geschehen soll, wenn du diesen Code antippst.

  Statt eines konkreten OID-Codes kann auch ein Code-Name angegeben werden, siehe Abschnitt „:ref:`code-namen`“.

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
erlaubt, Texte automatisch vorgelesen zu bekommen. So kannst du deine Tiptoi-Entwicklung testen, bevor du alles Nötige aufgenommen hast.

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
werden (und bereits gedruckte Codes weiterhin funktionieren), speichert es die
Auswahl in einer Datei mit Endung ``.codes.yaml``, die nur den
``scriptcodes``-Eintrag enthält. Es steht dir frei, diese Zuordnung in die
eigentliche YAML-Datei zu übernehmen.

.. warning::

   Das ``tttool`` arbeitet *entweder* mit Namen *oder* mit Nummern. Du kannst
   die beiden Varianten nicht mischen.


.. _yaml-skripte:

YAML-Programmierung
~~~~~~~~~~~~~~~~~~~

Die Logik einer Tiptoi-Programmierung steckt vor allem in den im ``scripts``-Feld angegebenen Skripten. Es gibt zu jedem OID-Code ein Skript. Dieses besteht aus einer oder gegebenenfalls mehreren Zeilen, die wiederum aus Befehlen bestehen.

Das einfachste Beispiel ist also

.. code:: yaml

     scripts:
       2000: P(hallo)

Hier wird, wenn du den OID-Code 2000 antippst, der Befehl ``P(hallo)`` ausgeführt. (Die Befehle selbst werden in Kürze erklärt.)

Eine Skriptzeile kann mehrere Befehle enthalten, etwa

.. code:: yaml

     scripts:
       2000: P(hallo) P(freund) J(2001)

Hier werden drei Befehle nacheinander ausgeführt.

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

Tippst du nun Code 2000 an, wird die erste Zeile ausgeführt, deren Bedingungen alle erfüllt sind (mehr zum Programmieren mit Bedingungen im :ref:`Abschnitt zu Bedingungsbefehle <conditionals>`\ ).

Statt die OID-Codes numerisch anzugeben, kannst du auch sprechende Namen verwenden, siehe Abschnitt „:ref:`code-namen`\ “.

Register
^^^^^^^^
Viele Befehle manipulieren :index:`\ <Register>`\ *Register*. Diese
repräsentieren Speicherzellen, in denen im Programmverlauf Werte abgelegt und
abgerufen werden können. Man könnte sie auch Variablen nennen.

Der Name eines Registers beginnt immer mit einem `$`, gefolgt von Buchstaben,
Zahlen oder Unterstrichen (`_`). Direkt nach dem `$` muss ein Buchstabe kommen.

Alle Arithmetik auf dem Tiptoistift arbeitet mit ganzen Zahlen im Bereich 0 bis 65535). Alle Register haben zu Beginn den Wert 0, sofern du es nicht im ``init``-Feld anders verlangst (siehe Abschnitt „:ref:`yaml-init`\ “.

Wenn du eine GME-Datei exportierst (siehe Abschnitt „:ref:`tttool-export`\ “), so kennt das ``tttool`` die Namen der Register nicht. In diesem Fall werden Nummern verwendet (``$0``, ``$1``\ …). Es gibt in der Regel keinen Grund, dies in deinen eigenen Tiptoi-Produkten so zu machen.

Befehlsreferenz
~~~~~~~~~~~~~~~

Im Folgenden werden die Befehle im Einzelnen erklärt: Wie du sie in der YAML-Datei schreibst, was sie bewirken, und was sonst so dabei zu beachten ist.

In der Format-Beschreibung werden folgende Platzhalter verwendet:

* *audio-datei*: Der Name einer Audio-Datei. Aus dem Namen wird, wie im Abschnitt „:ref:`media-path`\ “ beschrieben, der Dateiname der Audiodatei abgeleitet.
* *oid-code*: Die Nummer eines OID-Codes (und damit einer Skiptzeile), wenn ``scriptcodes`` *nicht* verwendet wird.
* *code-name*: Der Name eines OID-Codes (und damit einer Skiptzeile), wenn ``scriptcodes`` verwendet wird.
* *register*: Der Name eines Registers, mit `$`. Beispiel: `$mode`.
* *argument*: Entweder der Name eines Registers oder eine Zahl. Beispiele: `$mode`, `0`, `1024`.

    Der *Wert* eines Argumentes ist im ersten Fall der aktuell in dem Register gespeicherte Wert; im zweiten Fall einfach die Zahl selbst.

.. _command-P:

``P`` – Audio abspielen
^^^^^^^^^^^^^^^^^^^^^^^

Format:
  | **P(**\ *audio-datei*\ **)**
  | **P(**\ *audio-datei*\ **,**\ *audio-datei*\ **,**\ …\ **)**
Beispiel:
  .. code:: yaml

    haus:
    - P(willkommen) P(zu_hause,daheim)

Effekt:
    In der ersten Form spielt der Befehl die angegebene Audio-Datei ab.

    In der zweiten Form spielt der Befehl zufällig eine der angegebenen Audio-Dateien ab.


.. _command-J:

``J`` – Sprung
^^^^^^^^^^^^^^

Format:
  | **J(**\ *oid-code*\ **)**
  | **J(**\ *code-name*\ **)**
Beispiel:
  .. code:: yaml

    endlos:
    - P(kein_anschluss) J(endlos)
Effekt:
  Der Stift führt, nach dem aktuellen Skript, das Skript mit Code *oid-code* bzw. *code-name* (wenn :ref:`scriptcodes <code-namen>` verwendet wird) aus.

.. warning::
  Die neuen Tiptoi-Stifte (die mit der Audiobook-Funktion) verhalten sich bisweilen seltsam, wenn **J**- und **P**-Befehle gemischt werden.

  TODO: Was genau passiert hier?


``:=`` – Register setzen
^^^^^^^^^^^^^^^^^^^^^^^^

Format:
  | *register1* **:=** *argument*
Beispiel:
  .. code:: yaml

    - $zuletzt := $aktuell  $aktuell := 5
Effekt:
  Der Wert des Registers *register* wird auf den Wert des Arguments *argument* gesetzt.

``+=``, ``-=``, ``*=``, ``/=``, ``%=`` – Arithmetik
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Format:
  | *register* **+=** *argument*
  | *register* **-=** *argument*
  | *register* ***=** *argument*
  | *register* **/=** *argument*
  | *register* **%=** *argument*
Beispiel:
  .. code:: yaml

    taste_fuenf:
    - $anzeige*=10 $anzeige+=5
    gegner_getroffen
    - $wert := 10 $wert *= $bonus $score += $wert
Effekt:
  Es wird die entsprechende Rechenoperation auf die aktuell in *register* gespeicherte Zahl und den Wert des Arguments *argument* angewandt, und das Ergebnis in *register1* abgelegt.

Es wird dabei nur mit ganzen Zahlen gerechnet. Insbesondere rundet die Division (**/=**) das Ergebnis stets ab. Wenn also Register `$x` den Wert 8 enthält und `$x/=3` ausgeführt wird, so enthält es den Wert 2.

Der Befehl **%=** berechnet entsprechend den Divisionsrest. Wenn also Register `$x` den Wert 8 enthält und `$x%=3` ausgeführt wird, so enthält es den Wert 2.


``Neg()`` –  Register negieren
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Format:
  | **Neg(**\ *register*\ **)**
Beispiel:
  .. code:: yaml

    - Neg($r)
Effekt:
  Der Wert des Registers *register* wird negiert: Aus 5 wird -5 und umgekehrt.



``&=``, ``|=``, ``^=`` – bitweise Operatoren
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Format:
  | *register* **&=** *argument*
  | *register* **|=** *argument*
  | *register* **^=** *argument*
Zweck:
  Es wird die entsprechende bitweise Operation auf die aktuell in *register*
  gespeicherte Zahl und den Wert des Arguments *argument* angewandt, und das Ergebnis
  in *register* abgelegt.

  Dabei ist **&=** das bitweise Und, **|=** das bitweise Oder, **^=** das
  bitweise exklusive Oder (XOR). Wenn dir das nichts sagt, brauchst du es
  vermutlich nicht.

``T`` – Timer
^^^^^^^^^^^^^

Format:
  | **T(**\ *register*\ **,**\ *modulus* **)**
Beispiel:
  .. code:: yaml

    wuerfel:
    - T($wurf,6)
Effekt:
  Der Wert des Tiptoi-Zählers zu Beginn des Skriptes wird (modulo dem *modulus*) im Register *register* abgelegt.

Der Tiptoi-Stift verfügt über einen Zähler, der während der Benutzung hochgezählt wird. Er wird schneller hochgezählt, wenn mit dem Stift interagiert wird, er ist also nicht zur Zeitmessung geeignet. Man kann damit aber (einfache) Zufallszahlen bekommen. Mehr dazu im Abschnitt :ref:`Zufallszahlen`.


.. _conditionals:

``==``, ``>=``, ``<=``, ``>``, ``<``, ``!=`` -- Bedingungen
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Format:
  | *argument1* **==** *argument2*
  | *argument1* **>=** *argument2*
  | *argument1* **<=** *argument2*
  | *argument1* **>**  *argument2*
  | *argument1* **<**  *argument2*
  | *argument1* **!=** *argument2*

Beispiel:
  .. code:: yaml

    haus:
    - $mode == 1? P(willkommen)
    - $mode == 2? $gefunden < 3? P(finde_mehr_steine)
    - $mode == 2? $gefunden == 3? P(raetsel_geloest)

Effekt:
   Bedingungsbefehle müssen stets am Anfang der Zeile stehen. Es wird der Wert
   des ersten Arguments entsprechend dem Vergleichsoperator mit dem zweiten
   Argument verglichen. Wenn alle Bedingungsbefehle einer Zeile zutreffen, dann
   wird die Zeile ausgeführt, sonst wird die nächste Zeile des Skriptes
   geprüft.

   Die Operatoren sind:

   ====== ===================
   Befehl Bedeutung
   ====== ===================
   **==** gleich
   **>=** größer oder gleich
   **<=** kleiner oder gleich
   **>**  echt größer
   **<**  echt kleiner
   **!=** ungleich
   ====== ===================

Weitere Befehle
^^^^^^^^^^^^^^^

(Befehle die der normale Tiptoi-Bastler nicht braucht, aber die das ``tttool`` ausspuckt)

* ``P*()``
* ``PA*()``
* ``PA*()``
* ``PA()``
* ``G()``
* ``C``
* ``?() ()``

