Tipps und Tricks
================

Hier kannst du zu konkreten Problemstellungen  nachlesen, wie du sie lösen kannst.

Zufallszahlen
-------------

Für Spiele und Rätsel mit dem Tiptoi-Stift ist es häufig nötig, Aktionen zufallsgesteuert auszuführen. Auch wenn bisher die Fähigkeiten des Stifts in dieser Hinsicht noch nicht ganz verstanden sind, gibt es ein paar Techniken, die man hier einsetzen kann.

### Timer

Der Stift verfügt über eine Art Timer, den man per Befehl `T()` abfragen kann:

    # Timer-Wert in $register speichern
    T($register, 65535)

**Syntax:** T(_register_, _modulo_)

 * _register_: Ziel der Berechnung
 * _modulo_: Der Timer-Wert wird per Modulo-Operation (Teilungsrest) auf den Bereich 0–_modulo_ begrenzt.

Die so erhaltenen Werte können unter Umständen bereits als Zufallszahlen eingesetzt werden, jedoch gibt es ein paar Probleme:

 * Aufeinanderfolgende Abrufe des Timers liefern immer eine monoton steigende Funktion.
 * Während der Stift inaktiv ist, läuft der Timer langsamer.
 * Ein zweimaliger Aufruf innerhalb der selben Anweisung liefert exakt den gleichen Wert.

Reicht der Timer nicht aus, kann man sich mit Pseudo-Zufallszahlen behelfen:

### Algorithmen zur Erzeugung von Pseudo-Zufallszahlen

(TODO)


Code-Muster in GIMP
-------------------
