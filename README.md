# Game Of Life – Gruppe 11


## Mitglieder

* Ben Riegel
* Jannik Winter


## Beschreibung

Dieses Projekt beschäftigt sich mit Conway's Game of Life.  
Es ist eine Simulation, die auf einem Gitter von 'toten' und 'lebenden' Zellen läuft.

Dafür sind folgende Regeln definiert:
* eine tote Zelle mit genau 3 lebenden Nachbarn wird zum Leben erweckt
* eine lebende Zelle mit 2 oder 3 lebenden Nachbarn bleiben am Leben
* eine lebende Zelle mit wengier als 2 lebenden Nachbarn stirbt an Einsamkeit
* eine lebende Zelle mit mehr als 3 lebenden Nachbarn stirbt an Überbevölkerung


## Funktionen

Folgende Funktionen können mit Tasten gesteuert werden:
* **F1** -> Spielfeld leeren
* **F2** -> Spielfeld zufällig generieren
* **F12** -> Zu nächster Zellfarbe wechseln
* **Leertaste** -> Start / Pause
* **Pfeiltaste Rechts** -> Step
* **R** -> Spielfeld zurücksetzen
* **G** -> Gitterlinien umschalten
* **N** -> Nächste Figur auswählen
* **Linke Maustaste** -> Ausgewählte Figur an Mausposition setzen


## Figuren

Folgende Figuren können im Spiel gesetzt werden:

### Statisch

* Block  
![Block](/images/block.png)
* Bee Hive  
![Bee Hive](/images/beehive.png)
* Loaf  
![Loaf](/images/loaf.png)
* Boat  
![Boat](/images/boat.png)
* Tub  
![Tub](/images/tub.png)

### Oszillierend

* Blinker  
![Blinker](/images/blinker.png)
* Toad  
![Toad](/images/toad.png)
* Beacon  
![Beacon](/images/beacon.png)
* Pulsar  
![Pulsar](/images/pulsar.png)
* Pentadecathlon  
![Pentadecathlon](/images/pentadecathlon.png)

### Spaceships
* Glider  
![Glider](/images/glider.png)
* Light-Weight-Spaceship  
![Light-Weight-Spaceship](/images/lws.png)
* Middle-Weight-Spaceship  
![Middle-Weight-Spaceship](/images/mws.png)
* Heavy-Weight-Spaceship  
![Heavy-Weight-Spaceship](/images/hws.png)

### Other
* Clock  
![Clock](/images/clock.png)
* Double-U  
![Clock](/images/doubleU.png)
* r-Pentomino  
![r-Pentomino](/images/rPentomino.png)
* Block Row  
![Block Row](/images/blockrow.png)
* Little Swimmer  
![Little Swimmer](/images/littleSwimmer.png)
* Swimmer  
![Swimmer](/images/swimmer.png)
* Glider Gun 1
* Glider Gun 2


### Starten 
Open Powershell -> cabal run
