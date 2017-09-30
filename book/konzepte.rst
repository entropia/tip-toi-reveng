Konzepte
========

Du hast das ``tttool`` erfolgreich :ref:`installiert <Installation>` und vielleicht schon ein paar :ref:`erste Schritte <erste_schritte>` gemacht? Dann ist es an der Zeit, einen vollständigen Überblick über die Arbeit mit dem Tiptoi-Stift zu bekommen.

Wie funktioniert der Stift?
---------------------------

In der Spitze des Tiptoi-Stifts steckt eine kleine Kamera. Wenn du mit dem Stift auf eine Seite eines Tiptoi-Buches tippst, so sucht die Kamera nach einem bestimmten Muster bestehend aus schwarzen Punkten. Dieses Muster ist sehr fein und mit bloßem Auge betrachtet fällt es kaum auf, aber wenn du genau hinschaust, kannst du es sehen.

Das Punktmuster selbst kodiert lediglich eine Zahl (zwichen 0 un 15000, um genau zu sein), den sogenannten :index:`*OID-Code* <OID-Code>`. Die eigentliche Logik des Stift -- also, was er wann sagt, also auch die eigentlichen Audio-Dateien -- ist in der GME-Datei gespeichert.

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
 * diese Zahlenwerte in sogennanten *:index:`Registern <Register>* ablegen und abrufen und
 * abhängig von diesen Werten unterschiedliche Proramm-Schritte abarbeiten.





Wozu das ``tttool``?
--------------------
