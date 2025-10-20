Tipps und Tricks
================

Hier kannst du zu konkreten Problemstellungen nachlesen, wie du sie
lösen kannst. Dieses Kapitel lebt auch von deinen Beiträgen!


.. _zufallszahlen:

Zufallszahlen
-------------

Für Spiele und Rätsel mit dem Tiptoi-Stift ist es häufig nötig, Aktionen
zufallsgesteuert auszuführen. Auch wenn bisher die Fähigkeiten des
Stifts in dieser Hinsicht noch nicht ganz verstanden sind, gibt es ein
paar Techniken, die du hier einsetzen kannst.

Zufälliges Abspielen von Audio-Dateien
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Wenn es nur darum geht, zufällig eine von mehreren Audio-Dateien
abzuspielen, genügt der ``P()``-Befehl mit mehreren Argumenten:

::

    - P(bing, plopp, peng)

Es ist nicht bekannt, wie der Zufallsgenerator hier funktioniert und wie
gleichmäßig die Verteilung ist. Manche Anwender haben beobachtet, dass
die erste Datei häufiger abgespielt wird.

(TODO: es wäre interessant zu dokumentieren, in welchen
Ravensburger-Produkten das vorkommt und wie es dort angewendet wird.)

Timer
~~~~~

Der Stift verfügt über eine Art Timer, den du per Befehl ``T()``
abfragen kannst:

::

    # Timer-Wert in $register speichern
    T($register, 65535)

**Syntax:** T(\ *register*, *modulo*)

-  *register*: Ziel der Berechnung
-  *modulo*: Der Timer-Wert wird per Modulo-Operation (Teilungsrest) auf
   den Bereich 0–\ *modulo* begrenzt.

Die so erhaltenen Werte kannst du unter Umständen bereits als
Zufallszahlen einsetzen, jedoch gibt es ein paar Probleme:

-  Aufeinanderfolgende Abrufe des Timers liefern monoton ansteigende
   Werte.
-  Während der Stift inaktiv ist, läuft der Timer langsamer.
-  Ein zweimaliger Aufruf innerhalb der selben Anweisung liefert exakt
   den gleichen Wert.

Reicht der Timer also nicht aus, kannst du dir mit Pseudo-Zufallszahlen
behelfen:

Algorithmen zur Erzeugung von Pseudo-Zufallszahlen
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Als Pseudo-Zufallszahlen bezeichnet man Reihen von Zahlen, welche aus
einer deterministischen Berechnung hervorgehen, und daher natürlich
nicht wirklich zufällig sind, aber wie zufällig erscheinen. Indem du den
Timer als Startwert (*seed*) verwendest, erhältst du eine in der Praxis
nicht vorhersagbare Zahlenfolge.

Eine einfache Implementation sieht beispielsweise folgendermaßen aus:

::

    random:
      - T($r,65535) $rnd+=$r $rnd*=25173 $rnd+=13849

Nach Aufruf von ``random`` befindet sich im Register ``$rnd`` eine
Zufallszahl zwischen 0 und 65535. Der Wertebereich lässt sich durch
Modulo beschränken:

::

    random:
      - T($r,65535) $rnd+=$r $rnd*=25173 $rnd+=13849 $wuerfel:=$rnd $wuerfel%=6 $wuerfel+=1

In diesem Beispiel erhält ``$wuerfel`` einen zufälligen Wert zwischen 1
und 6.

(TODO: Angaben darüber ergänzen, wie gut dieser Algorithmus
funktioniert) (TODO: weitere PRNG-Algorithmen)


.. _unterbrechenvonaudio:

Unterbrechen von Audio verhindern
---------------------------------

Wenn man auf einen Code tippt, der einen ``P()``-Befehl enthält, während der Tiptoi-Stift gerade eine Audio-Datei abspielt, dann wird normalerweise sofort die neue Audio-Datei abgespielt und das Abspiel der aktuellen Audio-Datei dadurch unterbrochen. Falls dies nicht erwünscht ist, so kannst du das Abspielen einer Audio-Datei folgendermaßen gegen solches Unterbrechen "schützen":

::

    code1:
      - $p==1?
      - $p:=1 J(_p0) P(audio1)
    code2:
      - $p==1?
      - $p:=1 J(_p0) P(audio2)
    ...
    _p0:
      - $p:=0

- Das Register ``$p`` hat den Wert 0, wenn gerade nichts abgespielt wird, und 1, während etwas abgespielt wird.
- Jedes Skript muss mit der Zeile ``- $p==1?`` beginnen. Falls ``$p`` den Wert 1 hat wird das Skript dann nicht weiter ausgeführt und ein laufendes Abspiel daher nicht unterbrochen.
- Ein Abspiel wird durch die Konstruktion ``$p:=1 J(_p0) P(audio)`` "geschützt". Dies bewirkt, dass der Wert im Register ``$p`` auf 1 gesetzt wird und die angegebene Audio-Datei abgespielt wird. Zudem wird **nach Ende des Abspiels** zum Skript ``_p0`` gesprungen. Dies nutzt das (noch nicht verstandene) Zusammenspiel von ``J()``-Befehl und ``P()``-Befehl, wobei der ``J()``-Befehl **vor** dem ``P()``-Befehl stehen muss!
- Das Skript ``_p0`` setzt das Register ``$p`` wieder auf den Wert 0 zurück.

**Beachte:**

- Zwei aufeinander folgende ``P()``-Befehle können durch die Konstruktion ``J(_p0) P(sag_dies) P(dann_das)`` leider **nicht** "geschützt" werden. Tippst du während des Abspiels der ersten Audio-Datei (``sag_dies``) auf einen OID-Code, so wird die zweite Audio-Datei (``dann_das``) nicht mehr abgespielt. Zum Skript ``_p0`` wird aber dennoch gesprungen, so dass das Register ``$p`` auf den Wert 0 zurückgesetzt wird. Varianten der ``P()``-Befehle wie etwa ``PA(sag_dies, dann_das)`` ändern daran nichts. Allerdings kann man sich behelfen, indem man eine neue Audio-Datei ``sag_dies_dann_das`` erstellt und nur einen Befehl ``P(sag_dies_dann_das)`` zum Abspielen nutzt.
- Während der Tiptoi-Stift eine Audio-Datei abspielt können durchaus OID-Codes angetippt werden und auch einfache arithmetische Befehle ausgeführt werden (z.B. den Wert in einem Register erhöhen), ohne das Abspiel zu unterbrechen. Es dürfen nur keine ``P()``-Befehle oder ``J()``-Befehle ausgeführt werden.
- Du kannst auch Skripte haben, die selbst "geschützte" Abspiele unterbrechen (etwa ein "Stop"-Skript)::

    stop:
      - $p:=0 P(nichts)

  Du musst dann aber darauf achten, das Register ``$p`` wieder auf 0 zurück zu setzen.

Hintergrundmuster
-----------------

Es kann optisch schöner sein, wenn nicht nur die aktiven Bereiche eines Tiptoi-Werkes mit OID-Codes versehen sind, sondern alle. Dazu kannst du ein neutrales Muster verwenden, das vom Tiptoi-Stift einfach ignoriert wird, und das auch ein laufendes Skript nicht unterbricht. Dieses Muster erzeugst du mit::

  $ ./tttool oid-code --raw 65535
