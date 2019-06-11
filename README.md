# NOZ/mh:n-Prognose zum Bedarf an U3-Kinderbetreuung 

Dieses GitHub-Repository anthält Daten und R-Skripte zum Artikel "Die ewige Lücke - So massiv unterschätzt die Regierung den Betreuungsbedarf für Kleinkinder ", erschienen am 12.06.2019 auf noz.de, shz.de und svz.de:

 
https://www.noz.de/artikel/1766791<br/>
https://shz.de/24225792<br/>
https://svz.de/24225872<br/>

## Datenquellen
Der R-Code ist sorgfältig kommentiert. Er lädt die Daten aus den im Ordner data liegenden Dateien. Die Originalquellen dieser Daten sind im R-Code oder in den Excel-Dateien selbst angegeben.

## Wie haben NOZ und mh:n die Betreuungszahlen prognostiziert?

NOZ und mh:n berechnen die nötigen Plätze für die Betreuung von U3-Kindern (Kinder im Alter von unter drei Jahren) in den Jahren 2017 bis 2025 prinzipiell nach derselben Methode wie die derzeit aktuellste Studie, auf die die Bundesregierung sich bezieht. Die Veröffentlichung mit dem Titel [Plätze. Personal. Finanzen – der Kita-Ausbau geht weiter](https://www.dji.de/fileadmin/user_upload/bibs2017/rauschenbach_schilling_plaetze_personal_finanzen.pdf) (PPF) wurde von einem Forschungsverbund der TU Dortmund (Arbeitsstelle „akjstat“) und dem Deutschen Jugendinstitut (DJI) erstellt.


Zwischen der Prognose von NOZ/mh:n und der des Forschungsverbunds gibt es zwei entscheidende Unterschiede:


1. Im Gegensatz zu PPF gehen NOZ und mh:n – dem Trend in der Vergangenheit folgend – von steigenden Elternwünschen für die U3-Betreuung aus.

2. NOZ und mh:n verwenden aktuellere – und gestiegene – Zahlen für bereits geborene U3-Jahrgänge als PPF, da diese Zahlen zum Zeitpunkt der PPF-Berechnungen noch nicht vorlagen. (Für die nötigen Betreuungsplätze ist der Effekt der steigenden Elternwünsche spürbar größer als der Effekt gestiegener Geburtenzahlen.)


NOZ und mh:n rechnen also die PPF-Prognose mit anderen Annahmen nach.


NOZ und mh:n haben ihre Prognose in der freien Programmiersprache R programmiert. Der komplette Code mit allen Datenquellen kann über das Datenportal Github angesehen, heruntergeladen und nachgerechnet werden.


Der NOZ/mh:n-Prognose liegen Angaben aus der PPF-Studie, Elternwünsche aus der DJI-Kinderbetreuungsstudie (KiBS) und Daten des Statistischen Bundesamtes zugrunde.


Basisjahr der PPF-Studie ist das Jahr 2016. Die Studie wurde zwar 2017 veröffentlicht, die letzten Ist-Werte darin liegen aber für 2016 vor. Eine neuere Studie, die den grundlegenden U3-Betreuungsbedarf prognostiziert, liegt der Bundesregierung nicht vor. Dies bedeutet, dass die Werte für 2017 und 2018 prognostizierte Werte sind, obwohl inzwischen eventuell bereits gemessene Ist-Werte vorliegen (etwa für den Elternwunsch oder die tatsächliche Betreuungsquote).


Auch die NOZ/mh:n-Prognose geht von 2016 als Basisjahr aus, um der PPF-Studie methodisch so ähnlich wie möglich zu sein, und den Unterschied herauszustellen, der vor allem durch die Elternwünsche nach Betreuung entsteht, die in der NOZ/mh:n-Prognose von 2017 bis 2025 dem Trend folgend steigenden.


Beide Prognosen berechnen die nötigen Betreuungsplätze als die Gesamtzahl der Kinder unter drei Jahren multipliziert mit dem Anteil dieser Kinder, der künftig betreut werden soll.


In der NOZ/mh:n-Prognose ist dieser Anteil der von den Eltern gewünschte Anteil an betreuten Kindern als so genannte „lineare Extrapolation“ der für die Jahre 2012 bis 2017 durch das DJI erhobenen Ist-Elternwünsche. Das heißt, dass die Elternwünsche jedes Jahr von 2017 bis 2025 genauso stark steigen, wie im durchschnittlichen Trend der Jahre 2012 bis 2017.


In der PPF-Studie spielen die Elternwünsche keine Rolle. Der zu betreuende Anteil an U3-Kindern berechnet sich ab 2017 so, als stiegen die realen Betreuungsquoten (die deutlich unterhalb der Elternwünsche liegen) genauso stark wie im Durchschnitt der Jahre 2012 bis 2017.


Da die realen Betreuungsquoten und auch die Elternwünsche je nach Region sehr verschieden sein können, unterscheiden sowohl NOZ/mh:n- als auch PPF-Prognose ihre Berechnungen für drei verschiedene Regionstypen:


1. Westliche Flächenländer mit dem schnellsten Anstieg der Elternwünsche: Schleswig-Holstein, Niedersachsen, Nordrhein-Westfalen, Hessen, Rheinland-Pfalz, Baden-Württemberg, Bayern und das Saarland

2. Östliche Flächenländer mit dem langsamsten Anstieg der Elternwünsche: Brandenburg, Mecklenburg-Vorpommern, Sachsen, Sachsen-Anhalt und Thüringen 3. Stadtstaaten mit mittlerem Anstieg der Elternwünsche: Hamburg, Bremen und Berlin

Die Prognose wird für diese drei Regionen jeweils separat berechnet. Für deutschlandweite Angaben werden die Ergebnisse summiert.
