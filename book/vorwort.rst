Vorwort
=======

Seit den frühen 2010er Jahren erfreut der Tiptoi-Stift aus dem Hause
Ravensburger Kinder in Deutschland und in aller Welt. Mit ihm entlockt
das Kind den schön gestalteten Tiptoi-Büchern und -Spielen allerlei
Töne, Geräusche, Musik und Geschichten. Offensichtlich steckt in dem
Stift ein kleiner Computer, auf dem kleine Programme ablaufen. Da stellt
sich dir sicherlich die Frage: Kannst du mit dem Stift noch mehr machen,
als nur auf die original Ravensburger-Produkte zu reagieren?

.. figure:: img/Tiptoi_spielbrett.png
   :align: center
   :figwidth: 60%
   :alt: Tiptoi-Stift mit Spielbrett

   Tiptoi-Stift mit Spielbrett [#bild]_

Ja, kannst du! Denn – nach einigem Reverse-Engineering der Dateien, die
man auf den Stift lädt, wenn man ein neues Produkt gekauft hat, sowie
der feinen Punktmuster, die auf die Produkte gedruckt sind -- bildete
sich eine kleine Gemeinschaft von Tiptoi-Bastlern, die allerlei kleine
und große eigene Tiptoi-Bücher und -Spiele entwickelt haben. Technisch
dreht sich dabei alles um das ``tttool``, ein Kommandozeilenprogramm mit
dem man die Tiptoi-Dateien und -Codes erzeugt.

Dieses Buch ist sozusagen das Handbuch zum ``tttool``, aber noch mehr:
Es erklärt dir allgemeines zur Funktionsweise des Stiftes und wie du
dein eigenes Tiptoi-Projekt verwirklichst.

Rechtliches
-----------

Eine häufig gestellte Frage ist: Darf ich das überhaupt. Diese Frage
lässt sich nicht einfach beantworten, und keiner der Autoren ist Jurist.

Was auf jeden Fall gar nicht geht, ist mit dem ``tttool`` ein
Tiptoi-kompatibles Produkt zu erstellen und so zu tun, als ob es von
Ravenburger ist.

Was sicherlich auch Ärger gibt, ist wenn du dein eiges Tiptoi-Produkt
erstellst und kommerziell vertreibst, oder damit etwas machst was der
Marke “Tiptoi” schadet -- etwa ein eindeutig für Kinder ungeeignetes
Thema.

Alles andere – Werke für den Eigengebrauch oder als Geschenk,
Anleitungen online, eine offene Diskussion über die Funktionsweise des
Stift – hat irgendwer schon mal gemacht. Wir gehen davon aus dass
Ravensburger davon weiß und sehen, dass Ravensburger uns stillschweigend
– vielleicht gar wohlwollend – gewähren lässt.


Grafische Tools
---------------

Das ``ttool`` ist ein Kommandozeilentool. Wer sich davon nicht
abschrecken lässt, kann tolle Sachen damit machen. Wer es einfacher haben
will, sollte sich folgende Projekte anschauen, die allesamt auf
``tttool`` aufbauen:

-  Andreas Grimme hat mit
   `ttaudio <https://github.com/sidiandi/ttaudio#readme>`__ eine
   Windows-GUI erstellt, falls man einfach nur ein paar Audio-Dateien
   auf den Stift laden will.
-  Till Korten hat mit
   `ttmp32gme <https://github.com/thawn/ttmp32gme>`__ eine grafische
   Anwendung (Windows, OS X und Linux) erstellt, die ebenfalls
   Audio-Dateien auf den Stift lädt, und sehr schöne Übersichten zum
   Antippen druckt.

Weiterführende Informationen
----------------------------

Das Ziel dieses Buches ist, dir alles nötige über die Bastelei mit dem Tiptoi-Stift zu erklären. Doch dieses Ziel wird vermutlich nie erreicht... solltest du also mehr Informationen brauchen, findest du sie hier:

* Du solltest unbedingt die `tiptoi-Mailingliste <https://lists.nomeata.de/mailman/listinfo/tiptoi>`_ abonnieren. Hier tauschen sich alle Tiptoi-Bastler aus, und irgendwer kann dir sicher weiterhelfen. Auch freuen wir uns sehr zu erfahren, was du so mit dem ``tttool`` auf die Beine gestellt hast.

* Fehler im tttool oder Verbesserungsvorschläge, sowohl zum ``tttool`` als auch zu diesem Buch, darfst du gerne über den `Github-Bugtracker <https://github.com/entropia/tip-toi-reveng/issues>` melden.


* Wenn du dich für technische Details interessierest, die das ``tttool`` eigentlich vor dir versteckt, so solltest du ins `Wiki der Github-Seite <https://github.com/entropia/tip-toi-reveng/wiki>`_ schauen.



.. [#bild] `CC BY-SA 3.0 <https://creativecommons.org/licenses/by-sa/3.0>`_, by
       Schwesterschlumpf, from Wikimedia Commons

