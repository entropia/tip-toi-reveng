Konzepte
========

Du hast das ``tttool`` erfolgreich :ref:`installiert <Installation>` und vielleicht schon ein paar :ref:`erste Schritte <erste_schritte>` gemacht? Dann ist es an der Zeit, einen vollständigen Überblick über die Arbeit mit dem Tiptoi-Stift zu bekommen.

Wie funktioniert der Stift?
---------------------------

In der Spitze des Tiptoi-Stifts steckt eine kleine Kamera. Wenn du mit dem Stift auf eine Seite eines Tiptoi-Buches tippst, so sucht die Kamera nach einem bestimmten Muster bestehend aus schwarzen Punkten. Dieses Muster ist sehr fein und mit bloßem Auge betrachtet fällt es kaum auf, aber wenn du genau hinschaust, kannst du es sehen.

Das Punktmuster selbst kodiert lediglich eine Zahl (zwichen 0 un 15000, um genau zu sein), den sogenannten :index:`\ <OID-Code>`\ *OID-Code*. Die eigentliche Logik des Stift -- also, was er wann sagt, also auch die eigentlichen Audio-Dateien -- ist in der GME-Datei gespeichert.

Daraus ergibt sich dass man den Stift umprogrammieren kann, indem man diese GME-Dateien ändert.

Was sind Anschaltfelder und Produkt-IDs?
----------------------------------------

Nun finden sich auf deinem Tiptoi-Stift sicherlich mehrere GME-Dateien. Woher weiß der Stift, in welcher er schauen muss? Dazu gibt es die :index:`*Produkt-IDs* <Produkt-ID>`!

Wenn du einfach ein Buch öffnest und mit dem Stift irgendwo hintippst, dann liest der Stift den entsprechenden OID-Code, weiß aber nicht was er damit anfangen soll, und er wird dich auffordern, das Anschaltfeld des Produktes anzutippen.

Jedes Produkt hat so ein Anschaltfeld, und es zeichnet sich dadurch aus, dass es einen OID-Code im Bereich 1 bis 1000 kodiert. Das ist gleichzeitig die Produkt-ID des Produkts, und jedes Tiptoi-Produkt hat eine eigene Produkt-ID.

Eine GME-Datei enthält auch eine Produkt-ID. Der Stift schaut nun in alle GME-Dateien, die du auf ihn geladen hast, und sucht die GME-Datei mit der entsprechenden Produkt-ID. Wenn er eine solche findet, lädt er sie. Wenn du nun ins Buch tippst, kann der Stift in dieser GME-Datei nachschauen, was er zu tun hat.

.. note:: Aktiviere doch ein Buch (z.B. den Bauernhof) und tippe dann auf Elemente in einem anderen Buch. Mit etwas Glück reagiert der Stift mit Bauernhof-Tönen. Wenn das passiert, dann wurde der gleiche OID-Code in verschiedenen Produkten verwendet. Das ist kein Fehler: Die Produkt-ID löst die Uneindeutigkeit auf.


Was steckt in einer GME-Datei?
------------------------------

Neben der Produkt-ID, wie gerade eben erklärt, sowie den Audio-Dateien, die der Stift abspielen kann (in der Regel als Ogg-Vorbis, Mono, 22050Hz, aber der Stift versteht auch andere Audioformate wie WAV und MP3) enthält er die Logik, was er wann abzuspielen halt.

In erster Näherung ist das eine einfache Tabelle, die zu jedem OID-Code die Audio-Datei angibt, die abzuspielen ist.

Aber das ist natürlich noch mehr, denn der Stift mach ja nicht immer das gleiche, wenn man auf ein Feld tippt. Tatsächlich enthält diese Tabelle zu jedem OID-Code ein kleines Computer-Programm, dass nach dem Tippen abläuft. Dieses Programm (oder :index:`*Skript* <Skript>`) kann
 * Audio-Dateien abspielen,
 * mit Zahlenwerten rechnen,
 * diese Zahlenwerte in sogennanten :index:`Registern <Register>` ablegen und abrufen und
 * abhängig von diesen Werten unterschiedliche Program-Schritte abarbeiten.

Die Werte, mit denen gerechnet werden, sind dabei 16-bit natürlich Zahlen (also 0 bis 65535). Auch die Register (man könnte sie auch Variablen oder Speicherzellen nennen) speichern jeweils genau eine solche Zahl.

Desweiteren kann eine GME-Datei noch folgendes Enthalten:

 * Die Sprache der GME-Datei (Deutsch, Englisch usw.). Sollte die GME-Datei eine Sprache nennen, so wird sie nur abgespielt, wenn der Stift auf die gleiche Sprache eingestellt ist.
 * Spiele. Die Logik mancher komplizierteren Abläufe („Finde alle Mäuse!“) sind fest im Stift eingebaut, und die GME-Datei benennt nur die relevanten Felder und Audio-Dateien. Diese Spiele sind von uns zum Teil noch nicht vollständig verstanden.
 * Binäre Programme. Dies sind Maschinenprogramme, die direkt auf dem Prozessor des Stiftes ausgeführt werden. Auch diese sind von uns noch nicht vollständig verstanden.

Praktisch alle Elemente einer GME-Datei werden über intern schnöde Nummern statt über sprechende Namen angesteuert. Insbesondere kann man aus einer GME-Datei nicht mehr die Original-Dateinamen der (oft über hundert) Audio-Dateien rekonstruieren.

Wozu das ``tttool``?
--------------------

Um nun dein eigenes Tiptoi-Produkt zu erstellen, musst du eine solche GME-Datei erstellen. Nun ist das GME-Format ein unhandliches Binärformat, was du ohne Hilfsmittel nicht bearbeiten kannst. Genau dafür gibt es das ``tttool``, welches das GME-Format lesen und schreiben kann.

Für die meisten Tiptoi-Bastler sind drei Hauptfunktionen wichtig:

1. Das ``tttool`` kann eine GME-Datei in seine Bestandteile -- die Audio-Dateien und die Beschreibung der Logik -- zerlegen. Die Logik wird dabei in einem (halbwegs) benutzerfreundlichem, textbasiertem Format (der sogenannten *YAML-Datei*) abgelegt, dass du direkt mit einem Texteditor bearbeiten kannst.

2. Natürlich beherrscht das ``tttool`` auch die andere Richtung, und kann aus einer YAML-Datei und den Audio-Dateien eine GME-Datei erstellen. Um die Entwicklung zu vereinfachen kann das ``tttool`` dabei fehlende Audio-Dateien durch mittels *Text-to-Speech* (Sprachsynthese) erzeugen.

  .. warning:: Weil nicht alle Details des GME-Formats verstanden sind, können bei der Umwandlung von der GME-Datei zur YAML-Datei und zurück Teile verloren gehen, insbesondere Spiele.

3. Die OID-Codes, die zum Druck eines eigenen Tiptoi-Produktes nötig sind, können per ``tttool`` im PNG- oder PDF-Format erstellt werden.

Darüber Hinaus versteht verfügt das ``tttool`` über eine Reihe von Möglichkeiten zur Analyse von GME-Dateien, die vor allem zum Verstehen des GME-Formats nützlich sind.

Im Detail werden die einzelnen Funktionen des Tools werden im Kapitel “:ref:`tttool`” erklärt.
