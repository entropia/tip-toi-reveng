---

layout: default

gallery:
- title: Selbstgemachte Tier-Figuren
  subtitle: Video von Pronwan, 4 Minuten
  link: https://www.youtube.com/watch?v=Yic57Y9VORA
  img: https://img.youtube.com/vi/Yic57Y9VORA/mqdefault.jpg

- title: Monkey Island 3 für den Tip-Toi
  subtitle: Video von Pronwan, 8 Minuten
  url: https://www.youtube.com/watch?v=UieoGOHULVw
  img: https://img.youtube.com/vi/UieoGOHULVw/mqdefault.jpg

- title: Erklärung zur Programmierung
  subtitle: Video von Pronwan, 24 Minuten
  link: https://www.youtube.com/watch?v=xlUr1eZKhWw
  img: https://img.youtube.com/vi/xlUr1eZKhWw/mqdefault.jpg

- title: Der neu Besprochene Weltatlas
  subtitle: Blog-Post von Joachim Breitner
  link: https://www.joachim-breitner.de/blog/641-Personalisierte_Tip-Toi-Datei_als_Geschenk
  img: /img/weltatlas.png

- title: Weihnachtsgeschenke verteilen
  subtitle: Blog-Post von Joachim Breitner
  link: https://www.joachim-breitner.de/blog/666-Geschenke_mit_dem_Tiptoi-Stift_verteilen
  img: /img/weihnachten.png

- title: Ein Taschenrechner
  subtitle: Blog-Post von Joachim Breitner
  link: https://www.joachim-breitner.de/blog/669-Ein_Tiptoi-Taschenrechner
  img: /img/taschenrechner.png

- title: Eine Outdoor-Schatzsuche
  subtitle: Blog-Post
  link: http://mycvs.org/post/110330262976/interactive-treasure-hunt-with-tip-toi-the
  img: /img/schatzsuche.png

- title: Ein Vokabeltrainer
  subtitle: Artikel in der c't 8/2015
  link: http://www.heise.de/ct/ausgabe/2015-8-Eigene-Buecher-und-Spiele-fuer-den-Tiptoi-vertonen-2578001.html
  img: /img/ct.png

- title: Das verlorene Schaf
  subtitle: Ralley von Micha Reischuck
  link: https://lists.nomeata.de/archive/tiptoi/2015/000774.html
  img: /img/schaf.png

- title: Piratenralley
  subtitle: Ralley von Micha Reischuck
  link: https://github.com/michote/Piraten-Geburtstag
  img: /img/piraten.png

- title: Personalisiertes Zoo-Puzzle
  subtitle: von Andy
  link: http://keiplan.blogspot.no/2015/05/tiptoi-zoo-puzzle.html
  img: /img/zoo.png

- title: Ein Sequencer
  subtitle: von Peter Schneider
  link: http://www.raketenwerfer.de/post/120870754563/tttool-step-sequencer
  img: /img/sequencer.png

- title: Die Kompassrose
  subtitle: Ausführliche Anleitung in der Make 6/2015
  link: http://www.heise.de/make/inhalt/2015/6/108/
  img: /img/make.png

- title: Jakobs Tiptoi-Buch
  subtitle: von Keke
  link: https://kekex.de/blog/2016/05/21/448
  img: /img/keke.jpg
- title: Familienbuch
  subtitle: von Achim
  link: https://lists.nomeata.de/archive/tiptoi/2016/001349.html
  img: /img/achim.jpg

- title: Forscher-Geburtstag
  subtitle: Ralley von Micha Reischuck
  link: https://github.com/michote/tiptoi-forscher-geburtstag
  img: /img/forscher.jpg

- title: Fotobuch
  subtitle: von Andreas Zwinkau
  link: http://beza1e1.tuxen.de/articles/tiptoi.html
  img: /img/fotobuch.jpg

- title: Mon Premier Crayon
  subtitle: französisches Buch von Laurent
  link: https://www.dropbox.com/sh/47x9preiew174fh/AAAyEMqAA0Vy4VvSI5zA7788a?dl=0
  img: /img/crayon.png

- title: Schatzsuche
  subtitle: mit Video, von Michael Thon
  link: https://github.com/m7thon/schatzsucheH4
  img: /img/m7thon.jpg

- title: Pias Buch
  subtitle: mit Homepage, von André Schmid
  link: http://tiptoi.as-webstyling.de/
  img: /img/pias-buch.jpg

- title: Bachelorarbeit
  subtitle: von Tabea Bratzke
  link: https://github.com/MachEsEinfach/tiptoi_Abschlussarbeit
  img: /img/bratzke.jpg

- title: Tier-ABC
  subtitle: von Sebastian Dumoulin
  link: https://www.dropbox.com/s/u2uup0ukq4oasks/meinTiere_alphabet_TipToi.zip
  img: /img/abc.png

- title: tip-Master
  subtitle: von Frank
  link: https://github.com/chemtech1/TipMaster
  img: /img/tipmaster.png

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

## Was kann ich hier machen?

Zur Inspiration eine Sammlung von netten Tiptoi-Basteleien:

<div class="gallery">
{% for image in page.gallery %}
<div class="box">
<a href="{{image.link}}"><img src="{{image.img}}" width="320" height="180"/></a>
<div class="title">{{image.title}}</div>
<div class="subtitle">{{image.subtitle}}</div>
</div>
{% endfor %}
<div class="flexbox-fix"></div>
<div class="flexbox-fix"></div>
<div class="flexbox-fix"></div>
</div>

### Sonstiges zur Einstimmug:

 * Der Podcast Modellansatz [interviewte Joachim Breitner](http://modellansatz.de/papierrechner) zu dem Projekt (80 Minuten).
 * Auf der [GPN'15](https://entropia.de/GPN15) in Karlsruhe hielt Joachim einen [Vortrag zum Projekt](https://entropia.de/GPN15:Der_Tiptoi-Stift), der auch [aufgezeichnet](https://media.ccc.de/browse/conferences/gpn/gpn15/gpn15-6687-der_tiptoi-stift.html#video) wurde (63 Minuten).
 * Micha Reischuck zeigt einem [Screencast](https://youtu.be/QtdlwmKgg70) wie man mit GIMP die OID-Codes über Bilder legt.
 * Bei den [Linux-Info-Tag '19](https://www.luga.de/Aktionen/LIT-2019/) in Augsburg hat Joachim den Einstieg in das Tiptoi-Basteln gezigt, der Vortrag wurde [aufgezeichnet](https://www.youtube.com/watch?v=HX1Fpx4fer8) wurde (48 Minuten).


<!-- 2019-04-14 offline?
 * [Gerald Backmeister](http://mamu.backmeister.name/programmierung-und-skripting/tiptoi-hacking-mit-tttool/) beschreibt, was man mit dem tttool machen kann, und wie man es unter Ubuntu installiert.
-->

## Grafische Tools

Das `tttool` ist ein Kommandozeilentool. Wer sich davon nicht abschrecken lässt kann tolle Sachen damit machen. Wer es einfacher haben will sollte sich folgende Projekte anschauen, die allesamt auf `tttool` aufbauen:

 * Andreas Grimme hat mit [ttaudio](https://github.com/sidiandi/ttaudio#readme) eine Windows-GUI erstellt, falls man einfach nur ein paar Audio-Dateien auf den Stift laden will.
 * Till Korten hat mit [ttmp32gme](https://github.com/thawn/ttmp32gme) eine grafische Anwendung (Windows, OS X und Linux) erstellt, die ebenfalls Audio-Dateien auf den Stift lädt, und sehr schöne Übersichten zum antippen druckt. Auf [ScienceBlogs.de](http://scienceblogs.de/astrodicticum-simplex/2018/03/15/die-sternengeschichten-als-hoerbuch-auf-dem-tiptoi-stift/) beschreibt er detailliert, wie man mit ttmp32gme arbeitet.

## Wie kann ich das machen?

Alles, was du über das Basteln mit dem Tiptoi und dem `tttool` wissen musst, findest du im „[*Tiptoi-Buch*](https://tttool.readthedocs.io/de/latest/)“. Insbesondere erfährst du dort:

 * Wie funktioniert der Tiptoi-Stift prinzipiell?
 * Wie installiere ich as `tttool`?
 * Wie kann ich in Tiptoi-Produkten die Töne mit eigenen austauschen?
 * Wie erstelle ich komplett eigene Tiptoi-Produkte?
 * Was gibt es beim Drucken zu beachten?

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

Falko Oldenburg hat eine Webseite erstellt, mit der sich die
Ravensburger-GME-Dateien einfacher finden und herunterladen lassen:
<http://tiptoi.vakat.de/>

Cleracruza hat ein [Cheat-Sheet zum
Debug-Modus](https://github.com/cleracruza/test-mode-cheat-sheet), mit dem man
sich vom Stift die erkannten Codes vorlesen kann, erstellt.

## Impressum und Datenschutzerklärung

Diese Webseite wird betrieben von:

Joachim Breitner  
Ehbühl 33  
71083 Herrenberg  

(Aber Fragen zum `tttool` sind auf der Tiptoi-Mailingliste besser aufgehoben, siehe Abschnitt „Community“.)

Der Betreiber dieser Webseite speichert und erhebt keine personenbezogenen
Daten. Die Webseite wird von on [GitHub pages](https://pages.github.com/)
gehostet; möglicherweise speichert und verarbeitet GitHub Ihre IP-Adresse
oder weitere Daten. Genaueres können Sie der [Datenschutzerklärung von
Github](https://help.github.com/articles/github-privacy-statement/) entnehmen.


Diese Website wurde mit [Jekyll](http://jekyllrb.com/) und dem Theme
[Solo](http://chibicode.github.io/solo/) erzeugt und wird von [Github
Pages](https://pages.github.com/) gehostet.

