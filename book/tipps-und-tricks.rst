Tipps und Tricks
================

Hier kannst du zu konkreten Problemstellungen nachlesen, wie du sie
lösen kannst.


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
      - T($r,65535) $rnd+=$r $rnd*=25173 $rnd+=13849 $wuerfel=$rnd $wuerfel%=6 $wuerfel+=1

In diesem Beispiel erhält ``$wuerfel`` einen zufälligen Wert zwischen 1
und 6.

(TODO: Angaben darüber ergänzen, wie gut dieser Algorithmus
funktioniert) (TODO: weitere PRNG-Algorithmen)

Ansage von Registerwerten
-------------------------

Code-Muster in GIMP
-------------------

Code-Muster in Adobe Photoshop
------------------------------

Druckereinstellungen
--------------------

(Einstellungen für Drucker, Liste kompatibler Drucker, Lösungen für
typische Probleme (FAQ?))
