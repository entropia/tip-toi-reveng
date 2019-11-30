.. _tttool:

Die ``tttool``-Befehle
======================

Das ``tttool``-Programm kann GME-Dateien zusammenbauen, zerlegen und die zugehörigen OID-Codes erstellen. Dazu unterstützt es eine Reihe von Unterbefehlen (``tttool assemble``, ``tttool export`` etc.), die wir im folgenden im Detail erklären.

Dieses Handbuch erklärt nur die Befehle, die für „normale Anwender“ relevant sind. Das Werkzeug unterstützt auch bei der Analyse von GME-Dateien (z.B. ``tttool explain``), diese Features sind allerdings nur für Entwickler relevant.

.. _tttool-assemble:


GME-Datei zusammenbauen
-----------------------

Format:
  | **tttool** **assemble** *eingabe.yaml*
  | **tttool** **assemble** *eingabe.yaml* *ausgabe.gme*

Beispiel:
  .. code::

    tttool assemble MeinTaschenrechner.yaml


Mit diesem Befehl baust du eine GME-Datei: Wenn *eingabe.yaml* eine gültige YAML-Datei, wie im Kapitel „:ref:`yaml-referenz`\ “ beschrieben ist, so schreibt das ``tttool`` die darin enthaltene Logik, zusammen mit den Audio-Dateien, in die Datei *ausgabe.gme*.

Wenn du keinen zweiten Dateinamen angibst, so schreibt das ``tttool`` die Ausgabe nach *eingabe.gme*.

Wenn die Datei *eingabe.yaml* benannte OID-Codes, wie sie in :ref:`der YAML-Referenz <code-namen>` beschrieben sind, enthält, so vergibt ``tttool`` selbstständig Code-Nummern. Damit sich diese bei weiteren Aufrufen von ``tttool assemble`` (or ``tttool oid-codes``, siehe unten) nicht ändern, merkt sich das ``tttool`` die Zuordnung, indem es die Datei *eingabe.codes.yaml* erzeugt. Diese solltest du nicht löschen.

.. _tttool-oid-codes:
.. _tttool-oid-code:
.. _tttool-oid-table:

OID-Codes erzeugen
------------------

Format:
  | **tttool** *Muster-Einstellungen* **oid-code** *code*
  | **tttool** *Muster-Einstellungen* **oid-code** *von*\ **-**\ *bis*
  | **tttool** *Muster-Einstellungen* **oid-codes** *eingabe.yaml*
  | **tttool** *Muster-Einstellungen* **oid-table** *eingabe.yaml*
  | **tttool** *Muster-Einstellungen* **oid-table** *eingabe.yaml* *ausgabe.pdf*
  | **tttool** *Muster-Einstellungen* **oid-table** *eingabe.yaml* *ausgabe.svg*

Mögliche Muster-Einstellungen:
  | **-**\ **-image-format** **PNG**
  | **-**\ **-image-format** **PDF**
  | **-**\ **-image-format** **SVG**
  | **-**\ **-image-format** **SVG+PNG**
  | **-**\ **-code-dim** *Größe*
  | **-**\ **-code-dim** *Breite*\ **x**\ *Höhe*
  | **-**\ **-dpi** *DPI*
  | **-**\ **-pixel-size** *Zahl*

Beispiel:
  .. code::

    tttool oid-code 998
    tttool --image-format PDF oid-code 0,50-100
    tttool oid-codes MeinTaschenrechner.yaml
    tttool --code-dim 20x20 oid-table MeinTaschenrechner.yaml

Das ``tttool`` kann OID-Muster in verschiedenen Formaten erzeugen -- das brauchst du dann, wenn du deine eigenen Tiptoi-Produkte gestalten willst. Es versteht dazu mehrere Befehle, je nach dem woher es wissen soll, zu welche Codes es die Muster erzeugen soll, und mehrere Optionen, die steuern, wie die Muster auszusehen haben.

OID-Codes auswählen
~~~~~~~~~~~~~~~~~~~

Wenn du einfach nur ein bestimmtes Muster erzeugen willst, so verwendest du den ``oid-code``-Befehl, und gibst das Muster an. Wenn du zum Beispiel

.. code::

  tttool oid-code 998

ausführst, erstellt dir ``tttool`` eine Datei ``oid-998.png`` (oder ``oid-998.svg``, wenn du SVG als Format ausgewählt hast).

Du kannst auch mehrere Codes und Code-Bereiche auf einmal auswählen::

  tttool oid-code 0,1,100-110


Aber oft willst du einfach alle Codes eines GME-Projektes erzeugen. Dazu kannst verwendest du ``tttool oid-codes``:::

  $ tttool oid-codes example.yaml
  Writing oid-42-START.png.. (Code 42, raw code 272)
  Writing oid-42-8065.png.. (Code 8065, raw code 3700)
  Writing oid-42-8066.png.. (Code 8066, raw code 3701)
  Writing oid-42-8067.png.. (Code 8067, raw code 3702)

Die erste Zahl im Dateinamen ist die Produkt-ID des Projekts, was dir helfen soll, die Dateien besser zuzuordnen. Wenn die YAML-Datei selbst explizit mit OID-Codes arbeitet, stehen diese auch im Dateinamen. Wenn du aber, wie in :ref:`der YAML-Referenz <code-namen>` erläutert, mit Code-Namen arbeitest, stehen die nachher auch im Dateinamen::

  $ tttool oid-codes example2.yaml
  Writing oid-42-START.png.. (Code 42, raw code 272)
  Writing oid-42-conditional.png.. (Code 13445, raw code 9250)
  Writing oid-42-hello.png.. (Code 13446, raw code 9251)
  Writing oid-42-registers.png.. (Code 13447, raw code 9252)


Zuletzt kannst du auch alle Codes eines Projektes in eine einzelne PDF- oder SVG-Datei schreiben. Die Datei enthält dann eine schlichte, übersichtliche Tabelle mit Feldern für alle Codes, was sehr geschickt während der Entwicklung deines Projektes sein kann – so kannst du deine Programm-Logik schon planen und testen, bevor du dich and die grafische Gestaltung machst. Du erstellst die Tabelle einfach mit::

  $ tttool oid-table example2.yaml

und findest danach eine ``example2.pdf`` im aktuellen Verzeichnis.

Datei-Formate
~~~~~~~~~~~~~

Das ``tttool`` unterstützt folgende Formate für die Muster

* PNG (mittels ``--image-format PNG``) ist ein pixelbasiertes Bildformat. Es eignet sich gut wenn du dein Projekt mit einem Bildverarbeitungsprogramm wie GIMP oder Photoshop erzeugst. Achte darauf dass du das Bild nach dem Import in dein Programm nicht skalierst oder drehst, sondern allenfalls zuschneidest. PNG ist das Standardformat für ``tttool oid-code`` und ``tttool oid-codes``, und wird von ``tttool oid-table`` nicht unterstützt.
* SVG (mittels ``--image-format SVG``) ist ein Vektor-Format, und eigentlich sich gut für die Weiterverarbeitung in Zeichenprogrammen wie Inkscape oder Illustrator. So kann man zum Beispiel mit ``tttool --image-format SVG oid-table`` eine SVG-Datei mit allen Mustern erzeugen, und diese dann weiterverarbeiten. SVG wird von allen Befehlen unterstützt.
* SVG mit PNG (mittels ``--image-format SVG+PNG``) ist eine Variante, bei der zwar als SVG-Dateien erzeugen werden, aber in der SVG-Datei das Muster selbst als PNG-Datei angelegt ist. Dies kann, je nach verwendetem Programm und Drucker, eventuell zu besser erkennbaren Mustern führen.
* PDF (mittels ``--image-format PDF``) wird nur von ``tttool oid-table`` unterstützt und ist dort auch die Standardeinstellung, und eignet sich gut zum Drucken der Tabelle, jedoch nur bedingt für die Weiterverarbeitung.

Muster-Einstellungen
~~~~~~~~~~~~~~~~~~~~

Mit folgenden Optionen kannst du das nachjustieren, wie das Muster erstellt wird -- je nach Drucker funktionieren andere Einstellungen besser.

* Mit der Option ``--code-dim`` legst du fest, wie groß das Muster erzeugt werden soll. Du kannst entweder eine Zahl angeben, dann bekommst du ein Quadrat mit der angegebenen Seitenlänge in Millimeter, also ``--code-dim 30`` für ein 3×3cm Quadrat (dies ist die Standard-Einstellung). Oder du gibst mit zwei Zahlen die Breite und Höhe an, etwa ``-code-dim 210x297`` für ein Muster in A4-Größe.

* Die Option ``--dpi`` gibt die gewünschte Auflösung des Musters an, in der im Druck üblichen Einheit Punkt-Pro-Zoll (dots per inch). Der Standardwert ist 1200 DPI, unter Umständen genügen auch 600 DPI.

* Die Option ``--pixel-size`` gibt an aus wievielen Pixel (im Quadrat) ein Punkt des Musters gebaut werden soll. Der Standardwert ist 2. Wenn du diese Zahl erhöhst bekommst du ein kräftigeres, schwärzeres Muster, das zwar stärker auffällt, aber vielleicht besser erkannt wird.

.. _tttool-export:
.. _tttool-media:

GME-Dateien extrahieren
-----------------------

Format:
  | **tttool** **export** *eingabe.gme*
  | **tttool** **export** *eingabe.gme* *ausgabe.yaml*
  | **tttool** **media** *eingabe.gme*
  | **tttool** **media** *eingabe.gme* **-d** *verzeichnis*

Beispiel:
  .. code::

    tttool export WWW_Bauernhof.gme
    tttool media WWW_Bauernhof.gme

Du kannst eine GME-Datei entpacken, und sowohl die Audio-Dateien als auch die die Logik in Form einer YAML-Datei extrahieren. Dies geschieht mit zwei Befehlen:

Der Befehl ``tttool export WWW_Bauernhof.gme`` schreibt die Logik in der GME-Datei in die Datei ``WWW_Bauernhof.yaml``, bzw. in die angegebene Ausgabedatei.

Der Befehl ``tttool media WWW_Bauernhof.gme`` schreibt alle Audio-Dateien in der GME-Datei als separate Dateien, meist im OGG-Vorbis-Format mit Dateiendung ``.ogg`` in das Unterverzeichnis ``media``. Du kannst auch ein anderes Verzeichnis mittels ``-d`` angeben, aber beachte dann die ``media-path``-Einstellung in der YAML-Datei anzupassen, denn die vom ``tttool export```-Befehl erstellte YAML-Datei verweist standardmäßig auf ``media``.

.. _tttool-set-language:


Sprache einer GME-Datei ändern
------------------------------

Format:
  | **tttool** **set-language** *sprache* *datei.gme*
  | **tttool** **set-language** **--empty** *datei.gme*

Beispiel:
  .. code::

    tttool set-language FRENCH WWW_Bauernhof.gme

Um eine sprachspezifische GME-Datei in einem Stift zu benutzen, der auf eine
andere Sprache eingestellt ist, kann man die Sprache in der GME-Datei mit diesem Befehl ändern. Anders als via ``export`` und ``assemble`` bleibt so alle Funktionalität erhalten.

Typische Sprachangaben sind ``GERMAN``, ``FRENCH``, ``RUSSIA``.

Achtung: Der Befehl überschreibt die angegebene GME-Datei.
