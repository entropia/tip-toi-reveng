---
layout: default
---

Hier finden Sie Information zum tttool, einem Werkzeug zum analysieren und
erstellen von GME-Dateien für den Tiptoi-Stift. Damit können Sie
Tiptoi-Produkte umprogrammieren und eigene Produkte erstellen.

> **Achtung:** Das tttool ist kein offizielles Produkt von Ravensburger,
> sondern von unabhängigen Bastlern entwickelt. Wenn Sie damit ihren
> Tiptoi-Stift kaputt machen, ist das Ihr Problem. Und wenn Sie selbst
> erstellte Tiptoi-Produkte verkaufen, verletzen Sie vermutlich eine Reihe von
> Patenten und anderen Schutzrechten.

## Installation

Die Zip-Dateien auf der [Release-Seite] enthält die Datei `tttool.exe`, die man
direkt ausführen kann.


Allerdings ist zu beachten, dass das ein Kommadozeilenprogramm ist. Doppelt
klicken bringt also nichts, sondern man muss die Eingabeaufforderung starten,
in das Verzeichnis mit `tttool.exe` wechseln und dann Befehle wie `tttool info
WWW_Bauernhof.gme` eintippen. Wem das neu ist sei ein kleines [Tutorial zur
Kommandozeile] empfohlen.

Wer kein Windows verwendet oder aus anderen Gründen das `tttool` selber
kompilieren will findet die Anleitung dazu in der [README des
Github-Projektes].

[Release-Seite]: https://github.com/entropia/tip-toi-reveng/releases
[Tutorial zur Kommandozeile]: http://www.owih.org/2012/03/04/xp-kommandozeile-teil-1/
[README des Github-Projektes]: https://github.com/entropia/tip-toi-reveng#installation

## GME-Dateien analysieren

Das `tttool` stellt eine Reihe von Befehlen bereit; die komplette Liste sieht
man, wenn man einfach nur `tttool` ausführt.

Die Befehle zur Analyse von GME-Dateien erwarten, dass man den Dateinamen mit
eintippt. Hat man etwa die Datei `WWW_Bauernhof.gme` in das gleiche Verzeichnis
wie `tttool` kopiert, kann man sich mit `tttool info WWW_Bauernhof.gme` ein
paar Informationen anzeigen.

Besonders interessant sind die folgenden Befehle.

 * `tttool media Dateiname.gme`:

   Dieser Befehl extrahiert alle Audio-Dateien in der GME-Datei und legt sie
   fortlaufend durchnummeriert im Unterverzeichnis `media` ab.

 * `tttool scripts Dateiname.gme`:

   Dies gibt die in der GME-Datei gespeicherten Befehle für die einzelnen
   optischen Codes aus. Das Befehlsformat wird im nächsten Abschnitt erklärt.

 * `tttool explain Dateiname.gme`

   Dieser Befehl gibt die GME-Datei als Hex-Code aus, wobei bekannte Abschnitte
   erläutert sind.

 * `tttool export Dateiname.gme`

   erstellt die Datei `Dateiname.yaml`, die den (von `tttool` verstandenen)
   Inhalt der Datei in menschenlesbarer und editierbarer Form enthält. Dies
   kann als Ausgangspunkt für den nächsten Abschnitt genommen werden.

## Eigene GME-Dateien produzieren

Mit dem `tttool` kann man auch komplett eigene GME-Dateien erstellen.
Dazu wird immer eine YAML-Datei benötigt. In der wird festgelegt, was der
Tiptoi-Stift in welcher Situation machen soll.

[YAML] ist ein generisches Datenformat, das man mit einem beliebigen Texteditor
erstellen und bearbeiten kann. Dabei ist zu beachten, dass in YAML
Einrückungen, also Leerzeichen am Anfang der Zeile, wichtig sind.

[YAML]: http://de.wikipedia.org/wiki/YAML

Als Ausgangspunkt kann eine YAML-Datei dienen, die man mit `tttool export` aus
einer existierenden GME-Datei bekommt, oder die knapp gehaltene Beispieldatei
[`example.yaml`], die auch in der ZIP-Datei enthalten ist. 

[`example.yaml`]: https://github.com/entropia/tip-toi-reveng/blob/master/example.yaml

Die Umwandlung der YAML-Datei in eine GME-Datei geschiet mit dem Befehl

    tttool assemble mein_produkt.yaml

Wenn es keine Fehler gab liegt danach eine `mein_produkt.gme` im Verzeichnis, die man auf den Stift kopieren kann. Diese Schritte muss man natürlich nach jeder Änderung an der YAML-Datei oder den Audio-Dateien neu durchführen.

### Eine einfache GME-Datei

In der Datei legt man zuerst ein paar allgemeine Einstellungen fest. Die einzig zwingend notwendige ist die `product-id`. Hier wird der Codes des *Einschaltknopfes* festgelegt, den jedes Tiptoi-Produkt hat. Will man ein vorhandenes Tiptoi-Produkt neu besprechen, muss man natürlich den Einschaltcode dieses Produktes nehmen. Erstellt man etwas komplett neues sollte man hier eine Nummer nehmen, die keinem offiziellen Produkt entspricht. Zur Zeit bietet sich da die 42 an.

Damit man mitbekommt, dass der Stift das eigene Produkt auch erkannt hat, kann man im Feld `welcome` einen oder, durch Kommas getrennt, mehrere Dateinamen von Audio-Dateien angeben. Diese Dateien sollten im gleichen Verzeichnis wie die YAML-Datei liegen, und im OGG- oder MP3-Format sein. In der YAML-Datei lässt man die Dateiendung allerdings weg. 

Die eigentliche Logik landet dann in dem Abschnitt `scripts`. Hier gibt man, mit Einrückung, die Codes des Tip-Toi-Produktes an und zu jedem Code, mit Spiegelstrichen aufgelistet, die auszuführenden Skripte. Dies lässt sich am besten an einem Beispiel illustrieren:

~~~
product-id: 42
welcome: hallo
scripts:
  8066:
  - P(erstes_feld)
  8067:
  - P(zweites_feld)
~~~

Diese Datei definiert ein Produkt mit Einschaltcode 42. Wenn man es aktiviert spielt der Stift die Datei `hallo.ogg` (oder `hallo.mp3`, wenn er das findet) ab. Es gibt zwei aktive Felder, mit Codes 8066 resp. 8067. Wenn man auf diese Felder geht wird `erstes_feld.ogg` resp. `zweites_feld`.ogg` abgespielt.

Man kann mit dem *Play-Befehl* `P` auch mehrere Dateien angeben, etwa `P(gut,super,toll)`, dann wird jedesmal zufällig eine der Dateien `gut.ogg`, `super.ogg` oder `toll.ogg` abgespielt.

### Komplexere Abläufe mit Registern

Will man den Stift auch wirklich interaktiv machen, so kann man in den YAML-Dateien kleine Programme schreiben. Der Stift unterstützt einfache Register-Befehle. Register schreibt man `$` gefolgt von einem Namen, etwa `$modus`. In den Skripten, also da, wo oben der `P`-Befehl steht, kann man die Register abfragen und verändern.

Eine Abfrage sorgt dafür, dass diese Skriptzeile nur dann weiter ausgeführt wird, wenn die Bedingung erfüllt ist. Die Zeile

~~~
  8066:
  - $modus==1? P(wir_spielen)
~~~

spielt `wir_spielen.ogg` nur dann ab, wenn das Register `$modus` auf 1 gesetzt ist. Daher macht es oft Sinn, mehrere Zeilen pro Code zu verwenden:

~~~
  8066:
  - $modus==1? P(wir_spielen)
  - $modus==2? P(wir_hoeren_zu)
~~~

Neben der Abfage `$register == zahl?` gibt es noch die üblichen Vergleichsoperatoren `!=`, `>`, `<`, `>=` und `<=`.

Um ein Register zu setzen schreibt man etwa `$modus := 2`. Ein Code, der zwischen drei Modi durchschaltet, sähe also wie folgt aus:

~~~
  8066:
  - $modus==1? $modus:=2 P(jetzt_spielen_wir)
  - $modus==2? $modus:=3 P(jetzt_hoeren_wir_zu)
  - $modus==3? $modus:=1 P(jetzt_gibt_es_geraeusche)
~~~

Neben `:=` kann man mit dem Befehl `$register+=zahl` auch eine Zahl auf ein Register draufaddieren und mit `$regiser-=zahl` davon abgezogen werden.

Richtig aufwendige Programmierung geht mit dem Jump-Befehl, mit dem der Stift zum Skript eines anderen Codes springt:

~~~
  8066:
  - $modus==1? J(8067)
  8067:
  - P(hallo)
~~~

Hier wird sowohl bei 8066 als auch bei 8067 die Datei `hallo.ogg` ausgegeben.


Es gibt sicher noch mehr Befehle, aber es sind noch nicht alle entschlüsselt.

## Eigene Tiptoi-Produkte erstellen

*TODO*

### Die Crux mit den OID-Codes

*TODO*

## Community

Wer mit dem `tttool` herumspielt sollte sich auf der [tiptoi-Mailingliste]
eintragen. Hier sind auch andere Bastler, die einem eventuell weiterhelfen
können. Auch freuen wir uns immer wenn Ihr erzählt was Ihr mit dem `tttool`
auf die Beine gestellt habt.

[tiptoi-Mailingliste]: https://lists.nomeata.de/mailman/listinfo/tiptoi

Wenn Ihr Fehler im `tttool` findet oder Verbesserungsvorschläge habt, dürft Ihr
die über den [Github-Bugtracker] melden.

[Github-Bugtracker]: https://github.com/entropia/tip-toi-reveng/issues

Ansonsten wird der Tiptoi-Stift auch im [Mikrocontroller-Forum] besprochen.

[Mikrocontroller-Forum]: http://www.mikrocontroller.net/topic/214479

## Sonstiges

Dieses Website wurde mit [Jekyll](http://jekyllrb.com/) und dem Theme [Solo](http://solo.chibi.io) erzeugt und wird von [Github Pages](https://pages.github.com/) gehostet.
