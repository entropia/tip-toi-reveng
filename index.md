---
layout: default
---

Diese Seite enthält Information zum `tttool`, einem Werkzeug zum
Analysieren und Erstellen von GME-Dateien für den Tiptoi-Stift von
Ravensburger. Damit kann man Tiptoi-Produkte umprogrammieren und
eigene Produkte erstellen.

> **Achtung:** Das tttool ist kein offizielles Produkt von
> Ravensburger, sondern von unabhängigen Bastlern entwickelt. Wenn es
> zu einem Defekt am Tiptoi-Stift kommt, dann ist das zwar Pech, aber
> dennoch auf eigenes Risiko geschehen. Und wer selbst erstellte
> Tiptoi-Produkte verkauft, verletzt vermutlich eine Reihe von
> Patenten und anderen Schutzrechten.

## Was kann ich hier machen

Zur Inspiration eine Sammlung von netten Tiptoi-Basteleien:

 * Pronwan veröffentlich einige Videos:
   * [Selbstgemachte Tier-Figuren] (4 Minuten)
   * [Monkey Island 3] als Tiptoi-Spiel (8 Minuten)
 * Joachim Breitner, der Hauptentwicker des `tttool` berichtet auf seinem [Blog] von seinen Basteleien:
   * Der [neu besprochene Weltatlas]
   * [Weihnachtsgeschenke-Verteilung]
   * Ein [Taschenrechner]
 * Eine [Outdoor-Schatzsuche](http://mycvs.org/post/110330262976/interactive-treasure-hunt-with-tip-toi-the)
 * In der [c't 8/2015](http://www.heise.de/ct/ausgabe/2015-8-Eigene-Buecher-und-Spiele-fuer-den-Tiptoi-vertonen-2578001.html) wird beschrieben, wie man einen Vokabltrainer basteln kann.
 * [Rallye - Das verlorene Schaf](https://lists.nomeata.de/archive/tiptoi/2015/000774.html) von Micha Reischuck
 * [Ein personalisiertes Zoo-Puzzle](http://keiplan.blogspot.no/2015/05/tiptoi-zoo-puzzle.html) von Andy
 * [Ein Sequencer](http://www.raketenwerfer.de/post/120870754563/tttool-step-sequencer) von Peter Schneider.


[Selbstgemachte Tier-Figuren]: https://www.youtube.com/watch?v=Yic57Y9VORA
[Monkey Island 3]: https://www.youtube.com/watch?v=UieoGOHULVw
[Blog]: https://www.joachim-breitner.de/blog/
[neu besprochene Weltatlas]: https://www.joachim-breitner.de/blog/641-Personalisierte_Tip-Toi-Datei_als_Geschenk
[Weihnachtsgeschenke-Verteilung]: https://www.joachim-breitner.de/blog/666-Geschenke_mit_dem_Tiptoi-Stift_verteilen
[Taschenrechner]: https://www.joachim-breitner.de/blog/669-Ein_Tiptoi-Taschenrechner

## Installation (Windows)

Die Zip-Datei auf der [Release-Seite] enthält die Datei `tttool.exe`,
die man direkt ausführen kann.

Allerdings ist zu beachten, dass es sich dabei um ein
Kommandozeilenprogramm handelt. Doppelt klicken bringt also nichts,
sondern man muss die Eingabeaufforderung starten, in das Verzeichnis
mit `tttool.exe` wechseln und dann Befehle wie `tttool info
WWW_Bauernhof.gme` eintippen. Ein vorangestelltes `$` in folgenden
Listings wird nicht mit eingegeben, sondern markiert die Zeilen, die
einzugebenen sind.  Wem das neu ist, dem sei ein kleines [Tutorial zur
Kommandozeile] empfohlen.

Wer kein Windows verwendet oder aus anderen Gründen das `tttool`
selber kompilieren will, findet die Anleitung dazu in der [README des
Github-Projektes].

[Release-Seite]: https://github.com/entropia/tip-toi-reveng/releases
[Tutorial zur Kommandozeile]: http://www.owih.org/2012/03/04/xp-kommandozeile-teil-1/
[README des Github-Projektes]: https://github.com/entropia/tip-toi-reveng#installation

## GME-Dateien analysieren

Das `tttool` stellt eine Reihe von Befehlen bereit; die komplette
Liste sieht man, wenn man einfach nur `tttool` ausführt.

Die Befehle zur Analyse von GME-Dateien erwarten, dass man den
Dateinamen mit eintippt. Hat man etwa die Datei `WWW_Bauernhof.gme` in
das gleiche Verzeichnis wie `tttool` kopiert, kann man sich mit
`tttool info WWW_Bauernhof.gme` ein paar Informationen anzeigen.

Besonders interessant sind die folgenden Befehle.

 * `tttool media Dateiname.gme`:

   Dieser Befehl extrahiert alle Audio-Dateien in der GME-Datei und
   legt sie fortlaufend durchnummeriert im Unterverzeichnis `media`
   ab.

 * `tttool scripts Dateiname.gme`:

   Dies gibt die in der GME-Datei gespeicherten Befehle für die
   einzelnen optischen Codes aus. Das Befehlsformat wird im nächsten
   Abschnitt erklärt.

 * `tttool explain Dateiname.gme`

   Dieser Befehl gibt die GME-Datei als Hex-Code aus, wobei bekannte
   Abschnitte erläutert sind.

 * `tttool export Dateiname.gme`

   erstellt die Datei `Dateiname.yaml`, die den (von `tttool`
   verstandenen) Inhalt der Datei in menschenlesbarer und editierbarer
   Form enthält. Dies kann als Ausgangspunkt für den nächsten
   Abschnitt genommen werden.

## Eigene GME-Dateien produzieren

Mit dem `tttool` kann man auch komplett eigene GME-Dateien erstellen.
Dazu wird immer eine YAML-Datei benötigt. In der wird festgelegt, was
der Tiptoi-Stift in welcher Situation machen soll.

[YAML] ist ein generisches Datenformat, das man mit einem beliebigen
Texteditor erstellen und bearbeiten kann. Dabei ist zu beachten, dass
in YAML Einrückungen, also Leerzeichen am Anfang der Zeile, wichtig
sind.

[YAML]: http://de.wikipedia.org/wiki/YAML

Als Ausgangspunkt kann eine YAML-Datei dienen, die man mit `tttool
export` aus einer existierenden GME-Datei bekommt, oder die knapp
gehaltene Beispieldatei [`example.yaml`], die auch in der ZIP-Datei
enthalten ist.

[`example.yaml`]: https://github.com/entropia/tip-toi-reveng/blob/master/example.yaml

Die Umwandlung der YAML-Datei in eine GME-Datei geschieht mit dem
Befehl

    $ tttool assemble mein_produkt.yaml

Wenn es keine Fehler gibt, dann verrichtet `tttool` schweigend seinen
Dienst und anschliessend liegt eine Datei namens `mein_produkt.gme` im
Verzeichnis, die man auf den Stift kopieren kann. Diese Schritte muss
man natürlich nach jeder Änderung an der YAML-Datei oder den
Audio-Dateien neu durchführen. Wenn man eine bestehende GME-Datei als
Ausgangspunkt genommen hat, sollte man sicherstellen, diese nicht auch
noch auf dem Speicher des Stifts liegen zu haben. Ansonsten hat man
zwei Dateien für das gleiche Produkt (die gleiche `product-id`, s.u.)
vorliegen. Welche der Stift dann nimmt, ist dann Glückssache... Man
kann auch bei der nicht zu verwendenden Datei die Dateiendung `.gme`
auf irgendetwas anderes ändern, z.B. `.gmex`.

In der Eingabedatei legt man zuerst ein paar allgemeine Einstellungen
fest. Die einzig zwingend notwendige ist die `product-id`. Hier wird
der Code des *Einschaltknopfes* festgelegt, den jedes Tiptoi-Produkt
hat. Will man ein vorhandenes Tiptoi-Produkt neu besprechen, muss man
natürlich den Einschaltcode dieses Produktes nehmen. Erstellt man
etwas komplett neues, sollte man hier eine Nummer nehmen, die keinem
offiziellen Produkt entspricht. Erlaubt sind alle Zahlen von 1 bis
1000. Von Ravensburger unbenutzt ist zur Zeit die 42.

Damit man mitbekommt, dass der Stift das eigene Produkt auch erkannt
hat, kann man im Feld `welcome` einen oder - durch Kommas getrennt -
mehrere Dateinamen von Audio-Dateien angeben. Diese Dateien sollten im
gleichen Verzeichnis wie die YAML-Datei liegen, und im OGG- oder
MP3-Format sein. In der YAML-Datei lässt man die Dateiendung
allerdings weg.

Die eigentliche Logik landet dann in dem Abschnitt `scripts`. Hier
gibt man - mit Einrückung - die Codes des Tiptoi-Produktes an und zu
jedem Code, mit Spiegelstrichen aufgelistet, die auszuführenden
Skripte. Dies lässt sich am besten an einem Beispiel illustrieren:

~~~
product-id: 42
welcome: hallo
scripts:
  8066:
  - P(erstes_feld)
  8067:
  - P(zweites_feld)
~~~

Diese Datei definiert ein Produkt mit Einschaltcode 42. Wenn man es
aktiviert, spielt der Stift die Datei `hallo.ogg` (oder `hallo.mp3`,
falls er die .ogg-Datei nicht findet) ab. Es gibt zwei aktive Felder,
mit Codes 8066 resp. 8067. Wenn man auf diese Felder geht, wird
`erstes_feld.ogg` bzw. `zweites_feld.ogg` abgespielt.

Man kann mit dem *Play-Befehl* `P` auch mehrere Dateien angeben, etwa
`P(gut,super,toll)`, dann wird jedesmal zufällig eine der Dateien
`gut.ogg`, `super.ogg` oder `toll.ogg` abgespielt.

### Komplexere Abläufe mit Registern

Will man den Stift auch wirklich interaktiv machen, so kann man in den
YAML-Dateien kleine Programme schreiben. Der Stift unterstützt
einfache Register-Befehle. Register schreibt man `$` gefolgt von einem
Namen, etwa `$modus`. In den Skripten, also da, wo oben der `P`-Befehl
steht, kann man die Register abfragen und verändern.

Eine Abfrage am Zeilenanfang sorgt dafür, dass die jeweilige
Skriptzeile nur dann weiter ausgeführt wird, wenn die Bedingung
erfüllt ist. Die Zeile

~~~
  8066:
  - $modus==1? P(wir_spielen)
~~~

spielt `wir_spielen.ogg` nur dann ab, wenn das Register `$modus` auf 1
gesetzt ist. Daher wird man of mehrere Zeilen pro Code verwenden:

~~~
  8066:
  - $modus==1? P(wir_spielen)
  - $modus==2? P(wir_hoeren_zu)
~~~

Neben der Abfage `$register == zahl?` gibt es noch die üblichen
Vergleichsoperatoren `!=`, `>`, `<`, `>=` und `<=`. In einer Zeile
müssen immer erst *alle* Bedingungen erfüllt sein, bevor die Befehle rechts des Fragezeichens ausgeführt werden.

Um ein Register zu setzen, schreibt man etwa `$modus := 2`. Ein Code,
der zwischen drei Modi durchschaltet, sähe also wie folgt aus:

~~~
  8066:
  - $modus==1? $modus:=2 P(jetzt_spielen_wir)
  - $modus==2? $modus:=3 P(jetzt_hoeren_wir_zu)
  - $modus==3? $modus:=1 P(jetzt_gibt_es_geraeusche)
~~~

Neben `:=` kann man mit dem Befehl `$register+=zahl` auch eine Zahl
auf ein Register aufaddieren und mit `$regiser-=zahl` davon
abziehen.

Statt einer `zahl` kann auf der rechten Seite einer Zuweisung oder
eines Befehles auch ein Register stehen, etwa `$punkte +=
$gefundene_objekte`.

Richtig aufwendige Programmierung geht mit dem Jump-Befehl, mit dem
der Stift zum Skript eines anderen Codes springt:

~~~
  8066:
  - $modus==1? J(8067)
  8067:
  - P(hallo)
~~~

Hier wird sowohl bei 8066 als auch bei 8067 die Datei `hallo.ogg` ausgegeben.

Es gibt sicher noch mehr Befehle, aber es sind noch nicht alle
entschlüsselt.

### Namen statt Codes

Beim Erstellen eigener Tiptoi-Produkte sind die konkreten Codes für
die Skripte nicht so wichtig. Dann bietet es sich an, den Skripten
hilfreiche Namen zu geben, etwa statt

~~~
product-id: 42
welcome: hallo
scripts:
  8066:
  - P(erstes_feld)
  8067:
  - P(zweites_feld)
~~~

schreibt man 

~~~
product-id: 42
welcome: hallo
scripts:
  gruene_box:
  - P(erstes_feld)
  rote_box:
  - P(zweites_feld)
~~~

Wenn man diese Datei mit `tttool assemble` verarbeite, dann vergibt
`tttool` selbst die Codes für die Skripte. Die Zuordnung wird
gespeichert (Datei `...codes.yaml`), so dass man die Datei auch später
noch problemlos erweitern kann.

### Text-To-Speech

Gerade während man sein Tiptoi-Produkt noch entwickelt, oder für
schnelle Experimente, ist es eher hinderlich, wenn man jede
Audio-Ausgabe erst noch aufnehmen muss. Daher kann `tttool` auch
selbst Ansagen erstellen. Dazu gibt man zu jedem Sample den zu
sprechenden Text in einer eigenen Sektion `speak` an:

~~~
product-id: 42
welcome: hallo
scripts:
  gruene_box:
  - P(erstes_feld)
  rote_box:
  - P(zweites_feld)
speak:
  hallo: "Hallo"
  erstes_feld: "Dies ist das erste Feld."
  zweites_feld: "Dies ist das zweite Feld."
~~~

Die Trennung macht es einfach, dies später durch eigene Aufnahmen zu
ersetzen, indem man einfach die entsprechende Zeile in der
`speak`-Sektion auskommentiert.

## Eigene Tiptoi-Produkte erstellen

Neben dem eben beschriebenen Erzeugen einer GME-Datei benötigt man für
eigenen Produkte auch noch die ausgedruckten Codes. Diese erhält man
in drei einfachen Schritten:

 1. Mit `tttool oid-code` erstellt man die Punktmuster.
 2. Diese bringt man in die richtige Form und legt sie ggf. über ein Motiv.
 3. Das Ergebns druckt man.

Allerdings klappt insbesondere der letzte Schritt nicht immer problemlos.

### Codes erzeugen

Um einen Code zu erzeugen, etwa für den Start-Knopf für unsere selbst
erstellte GME-Datei mit Product-ID 42, führt man `tttool oid-code 42`
aus. Nach der Meldung

    Writing oid-42.png.. (Code 42, raw code 272)

findet man diese Datei im aktuellen Verzeichnis. Sie enhält das Muster
zum Code 42, vielfach wiederholt. Sie soll eine Fläche von 10×10cm
abdecken, was bei den empfohlenen 1200dpi eine Auflösung von 4800×4800
Pixeln ergibt.

Ähnliche Codes braucht man für jedes Feld, etwa `8066` oder `8067` im
obigen Beispiel. Man muss nicht alle einzeln erzeugen. `tttool` kann das
auch in einem Rutsch erledigen:

    $ ./tttool oid-code example.yaml 
    Writing oid-42-START.png.. (Code 42, raw code 272)
    Writing oid-42-8066.png.. (Code 8066, raw code 3701)
    Writing oid-42-8067.png.. (Code 8067, raw code 3702)

Hat man seinen Skripten Namen gegeben, wie oben beschrieben, dann
bekommt man auch schönere Dateinamen für die Muster:

    $ ~/projekte/tip-toi-reveng/tttool oid-code foo.yaml 
    Speaking "Hallo".
    Writing oid-42-START.png.. (Code 42, raw code 272)
    Writing oid-42-gruene_box.png.. (Code 4716, raw code 0)
    Writing oid-42-rote_box.png.. (Code 4717, raw code 1)

### Bilder erstellen

Wichtig beim Erstellen der Bilder ist die richtige Auflösung. Die
gerade erstellten Muster liegen in 1200dpi vor. Dies muss so bleiben;
sie dürfen nicht skaliert werden.

Daher empfiehlt es sich, mit einer leere Datei in der richtigen
Auflösung zu beginnen – zum Beispiel 10000×7000 Pixel für DIN-A5 – und
dann die eigenen Grafiken und Texte zu importieren und auf die
richtige Größe skalieren.

Fügt man in eine solche Datei die erstellten Dateien mit den
Punktmustern ein, so sollten sie tatsächlich eine Fläche von 10×10cm
abdecken, sonst hat man etwas falsch gemacht. Es empfiehlt sich, für
jedes Muster eine eigene Ebene zu verwenden, die man zuschneidet oder
mit einer Ebenenmaske auf die richtige Form und Größe bringt. Diese
Ebene darf man natürlich nicht skalieren!

### Produkt drucken

Ob der eigene Drucker die Codes gut abbilden kann, ist ein wenig
Glückssache. Mit manchen Druckern bekommt man auf Anhieb ein gut
brauchbares Ergebnis, mit anderen gar nicht.

Wenn es nicht gleich klappt, hier ein paar Tipps:

 * Spielen Sie mit den Druckeinstellungen herum. Können Sie die
   Auflösung erhöhen -- oder auch verringern? Probieren Sie 1200dpi
   oder 600dpi, wenn möglich. Ebenso können Sie den Kontrast
   verändern.
 * Funktionieren Schwarz-Weiß-Bilder besser als Farbbilder -- oder
   umgekehrt?
 * Vielleicht ist Ihr Motiv zu dunkel. Hellen Sie es ein wenig auf.
 * Es kann helfen, wenn die Fläche hinter den Codes nicht zu viel
   Struktur hat.  Färben Sie sie ggf. einfarbig ein.
 * Manche Drucker bringen die Codes nur dann brauchbar zu Papier, wenn
   nichts anderes gedruckt wird. Drucken Sie in dem Fall erst das
   Motiv und dann, aufs gleiche Blatt, die Codes. Das geht schnell
   indem Sie die jeweiligen Ebenen sichtbar bzw. unsichtbar machen.

Auch wenn Ihr Drucker nur die Codes ohne Motiv drucken kann, können
Sie viele schöne Sachen basteln. Schneiden Sie einfach die Codes aus
und bekleben Sie Spielsachen, Möbel, Bücher, etc.!

## Community

Wer mit dem `tttool` herumspielt, sollte sich auf der
[tiptoi-Mailingliste] eintragen. Hier sind auch andere Bastler, die
einem eventuell weiterhelfen können. Auch freuen wir uns immer, wenn
wir erfahren, was andere mit dem `tttool` auf die Beine gestellt
haben.

[tiptoi-Mailingliste]: https://lists.nomeata.de/mailman/listinfo/tiptoi

Wer tiefergehende Informationen zum Tiptoi-Stift sucht sollte auch mal ins
[Wiki der Github-Seite] schauen. Dort sind viele technische Details zum Stift
zusammengetragen worden.

[Wiki der Github-Seite]: https://github.com/entropia/tip-toi-reveng/wiki

Fehler im `tttool` oder Verbesserungsvorschläge dürfen gerne über den
[Github-Bugtracker] gemeldet werden.

[Github-Bugtracker]: https://github.com/entropia/tip-toi-reveng/issues

Ansonsten wird der Tiptoi-Stift auch im [Mikrocontroller-Forum] besprochen.

[Mikrocontroller-Forum]: http://www.mikrocontroller.net/topic/214479

## Sonstiges

Diese Website wurde mit [Jekyll](http://jekyllrb.com/) und dem Theme
[Solo](http://solo.chibi.io) erzeugt und wird von [Github
Pages](https://pages.github.com/) gehostet.
