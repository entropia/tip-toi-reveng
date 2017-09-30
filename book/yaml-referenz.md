YAML-Referenz
=============

In diesem Kapitel erfährst du alles über die `.yaml`-Datei, mit der du den
Tiptoi-Stift programmierst.

YAML-Format: Eine Übersicht
---------------------------

(Generelle Infos zu Yaml.)

Yaml-Datei-Felder
------------------

Um eine Yaml-Datei korrekt zu schreiben sind einige Strukturen einzuhalten.
Auf diese Strukturen möchten wir nun etwas näher eingehen.

Die Grundstruktur besteht dabei aus den folgenden Feldern
<ul>
<li>media-path:</li>
<li>language:</li>
<li>welcome:</li>
<li>product-id:</li>
<li>comment:</li>
<li>init:</li>
<li>scripts:</li>
<li>speak:</li>
</ul>

Diese Felder möchten wir dir nun näher erläutern.
<h3>media-pathy:</h3>
Im diesem Feld wird vorgegeben, wo sich die Audiofiles zum Programm befinden. Der Ordner wird hier inkremental angegeben. Wird hier beispielsweise "media/test_%s" geschrieben, so wird der String "%s" durch den Namen im Programm ersetzt.
Beispiel: Ein Aufruf mit P(audio) greift auf die Datei media/test_audio.ogg zurück.

<h3>language:</h3>
Wie der Name hier bereits vermuten lässt, lässt sich hier die Sprache des TipToi-Stiftes angeben. Für Deutsch wählen wir "de".

<h3>welcome:</h3>
Im Welcome-Feld wird auf eine Audiodatei verwiesen. Diese wird abgespielt, sobald unser Projekt aktiviert wird.
Beispiel: Wird hier "power" eingetragen, so wird die Datei "media/test_power.ogg" zum Start abgespielt.

<h3>product-id:</h3>
Der hier angegebene OID-Code startet das Projekt. Sollte von Ravensburger die Selbe ID genutzt werden, wird es auf dem Stift beim Starten des Projekts zu einem Konflikt kommen. Daher ist diese ID so zu wählen, dass Sie einmalig ist. Da Ravensburger fortlaufend ab 1 hochzählt, wird eine Zahl von 900 bis höher empfohlen. Somit bleibt uns noch eine lange Zeit, bis es zu einem Konflikt kommen könnte. Da 999 abwärts für Sprachen genutzt wird, rate ich zu einer ID von 900-950.

<h3>comment:</h3>
Dies ist das Kommentarfeld für uns Programmierer. Kommentiere dein Projekt nach Lust und Laune.

<h3>init:</h3>
In diesem Feld werden Strings initialisiert. Beispielsweise werden hier Spielmodi oder Zähler auf 0 gesetzt, damit diese Später mit einem vorgegebenen Wert starten können. 
Beispiel: Wird hier "$modus:=0 $i:=0" geschrieben, so wird nach Aktivierung des Projekts der String modus und i mit 0 gestartet.

<h3>scripts</h3>
Dies ist der eigentliche Hauptarbeitsbereich für unsere Projekte. In diesem Feld werden unsere Programmierungen geschrieben.

<h3>speak:</h3>
Das tttool verfügt über ein integriertes Text-2-Speech tool, welches es erlaubt Texte in Form von Audiofiles zu integrieren. Dies dient uns Beispielsweise während der Entwicklung dazu Texte nicht immer aufnehmen und einspeichern zu müssen.
Beispiel: hallo_welt: "Hallo Welt! Ich hoffe dir geht es gut."
Im Beispiel wird nun automatisiert ein Audiofile erstellt, welches mit "P(hallo_welt)" abgespielt werden kann.

Yaml-Skripte: Bedingungen und Befehle
-------------------------------------
