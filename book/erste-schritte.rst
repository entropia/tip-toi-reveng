Erste Schritte
==============

Dieses Kapiel erklärt dir Schritt für Schritt und an einfachen Beispielen, was du mit dem `tttool` anstellen kannst. Die Details werden dann vollständig in den nächsten Kapiteln erklärt.

Vorbereitung
------------

Diese Übersicht geht davon aus, dass du Windows verwendest. Wenn du Linux oder einen Mac benutzt, lies dir den Abschnitt :ref:`installation` durch.

Auf der
`Release-Seite <https://github.com/entropia/tip-toi-reveng/releases>`_ findest du eine Zip-Datei, die du herunterlädst und entpackst. Du findest darin die Datei ``tttool.exe``, die man direkt ausführen kann. Es ist keine weitere Installation nötig.

Allerdings ist zu beachten, dass es sich dabei um ein
Kommandozeilenprogramm handelt. Doppelt klicken bringt also nichts,
sondern du musst  die Eingabeaufforderung starten, in das Verzeichnis mit
``tttool.exe`` wechseln und dann Befehle wie
``tttool info WWW_Bauernhof.gme`` eintippen. Ein vorangestelltes ``$``
in folgenden Listings wird nicht mit eingegeben, sondern markiert die
Zeilen, die einzugebenen sind. Wenn dir das neu ist, dann sei dir ein kleines
`Tutorial zur
Kommandozeile <http://www.owih.org/2012/03/04/xp-kommandozeile-teil-1/>`__
empfohlen.

Töne in Tiptoi-Büchern ändern
-----------------------------

Als erstes einfaches Projekt kannst du in einem deiner Tiptoi-Bücher ein paar Töne ändern. Überrasche dein Kind doch in dem du deine eigene Stimme ertönen lässt! Als Beispiel nehmen wir das Buch „Wieso? Weshalb? Warum? -- Bauernhof“, aber du kannst diese Anleitung auch mit einem anderen Buch verfolgen.

Zu jedem Tiptoi-Produkt gehört eine „GME-Datei“, die sowohl die Töne als auch die Programmlogik enthält -- in unserem Fall ``WWW_Bauernhof.gme``. Du findest sie auf dem Tiptoi-Stift selbst, oder auf der `Ravensburger Download-Seite <https://www.tiptoi.com/de/start/anleitung-haendischer-download/index.html>`_. Kopiere Sie in den Ordner mit dem ``tttool.exe``.

Führe nun die folgenden beiden Befehle aus:

.. code:: bash

  $ tttool export WWW_Bauernhof.gme
  $ tttool media WWW_Bauernhof.gme

Du solltest nun in diesem Verzeichnis eine Datei ``WWW_Bauernhof.yaml`` finden (die wir vorerst ignorieren), sowie ein Verzeichnis ``media/`` mit vielen Audio-Dateien im ``ogg``-Format finden. Hör einfach mal rein!

Damit sind wir schon halb fertig. Suche die Audio-Datei, die du ersetzen willst, und merk dir den Dateinamen -- zum Beispiel ``WWW_Bauernhof_3.ogg`` für die Frage „Was ist das besondere an dem Kuhstall“.  Du kannst die Datei ruhig löschen, denn du willst sie durch deine eigene Aufnahme ersetzen.

Zu erklären, wie du eine Audiodatei aufnimmst, würde hier den Rahmen sprengen. Wichtig ist vor allem, dass du die Datei im OGG- oder MP3-Format aufnimmst. Speichere sie in dem ``audio``-Verzeichnis als ``WWW_Bauernhof_3.ogg``.

Nun musst du die GME-Datei wieder zusammenbauen:

.. code:: bash

  $ tttool assemble WWW_Bauernhof.yaml Mein_Bauernhof.gme

Nun solltest du in dem Verzeichnis die Datei ``Mein_Bauernhof.gme`` finden. Kopiere diese Datei auf den Tiptoi-Stift. Es darf immer nur eine GME-Datei pro Produkt auf dem Stift sein, also musst du die original ``WWW_Bauernhof.gme`` löschen -- natürlich nur nachdem du eine Sicherheitskopie auf deinem Rechner davon erstellt hast.


Das wars schon! Wenn du nun den Stift einschaltest, den Bauernhof aktivierst und auf den Kuhstall tippst, solltest du deine eigene Stimme hören.

.. note:: Da wir das Dateiformat nicht vollständig verstanden haben, kann es sein, dass manche Elemente -- insbesondere Spiele -- nun nicht mehr funkionieren.

Tiptoi-Bücher komplett neu vertonen
------------------------------------

(YAML-Datei schreiben, Text-to-Speech, Debug-Modus um Codepoints zu hören, `tttool assemble`)


Eigene Tiptoi-Bücher gestalten
------------------------------

(Benamte skripts, `ttool oid-table`, `tttool oid-codes`)
