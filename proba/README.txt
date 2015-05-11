Komponensalapu, Fa-Heurisztika kapcsolat.

Készítettem két példa komponenst. Az egyik a fa komponens utánzata, a másik egy heurisztika komponens (az Ataxx játékhoz).

Azt, hogy a fa komponens indítja el az oprendszeren keresztül a heurisztikát, kihagytam most, mert a fa úgysem java-ban lesz megírva.
Egyébként cmd-ből: java -jar Komponensalapu_heur.jar <portszám>

Futtatás:
---------

először egyik cmd-ből: java -jar Komponensalapu_treeProxy.jar
utána egy másik cmd-ből: java -jar Komponensalapu_heur.jar 6456

A 6456 a portszám amit a heur paraméterként kap meg indításkor. A portszám bele van kódolva a fa komponensbe és a heur ezen fog csatlakozni.
A portszámot a fa dönti el, tehát a heurisztikában nincs rögzítve egyáltalán.

Háromféle üzenet van:
---------------------
EVAL: a fa kéri a heur-t, hogy értékelje ki az üzenetben elküldött játékállapotot.
EVAL_RE: a heur válaszol, az üzenetben egy integer érték van, ami a játékállapot értéke.
CLOSE: a fa leállás előtt értesíti a heur-t evvel az üzenettel, hogy álljon le. Nem vár választ rá.

A példakód lefolyása:
---------------------
A szerver (fa) indításkor várakozik a 6456 porton, a kliens (heur) a program indításakor rögtön csatlakozni próbál ugyanitt.
Csatlakozás után a szerver 5 másodpercenként elküld egy játékállapotot tartalmazó EVAL üzenetet a heur-nak.
A heur minden megkapott EVAL üzenetre kiértékeli a kapott játékállapotot és válaszol egy EVAL_RE üzenettel aminek tartalma az állapot értéke.
A szerver egy perc után küld egy CLOSE üzenetet, majd leáll.
A heur a kapott CLOSE üzenet hatására leáll.

Megvalósítás:
-------------

TCP-n keresztül, JSON üzenetekkel kommunikál a két komponens.
A JSON üzeneteket az objektumokból a ./lib könyvtárban található json-io készíti el az automatikus szerializáció segítségével.
Az üzenetek pontos formátuma lejjebb. A lényeg, hogy kevés java specifikus információt tartalmaz az üzenet, mindössze package és osztálynév,
amit manuálisan is hozzá lehet tenni. Az üzenet többi része megfelel a JSON formátumnak.
A komponensekben a 'komp_common' csomagban megtalálható az Ataxx játékállapot reprezentációja és az üzeneteknek megfelelő java osztályok is.

Üzenetek részletesen:
---------------------
Az üzenetek végén egy \n is küldésre kerül, pl. java-ban a readLine() ezt nyilván leszedi...

EVAL:
{"@type":"komp_common.MessageFactory$MSG_Eval","state":{"board":[[1,0,0,0,0,0,0,2],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[2,0,0,0,0,0,0,1]],"nextPlayer":1},"messageType":"EVAL"}

EVAL_RE:
{"@type":"komp_common.MessageFactory$MSG_EvalRe","stateValue":1,"messageType":"EVALRE"}

CLOSE:
{"@type":"komp_common.MessageFactory$MSG_Close","messageType":"CLOSE"}


Minden üzenettípus rendelkezik tehát egy "messageType" String mezővel.
Az EVAL típusú üzenet ezenkívül rendelkezik egy "state" nevű objektummal, ami egy "board" nevű 8×8-as integer mátrixból és egy "nextPlayer" nevű integer mezőből áll.
Az EVAL_RE üzenetben a típus mezőn kívül van egy "stateValue" nevű integer mező.

Az EVAL_RE mindegyik játék esetén ugyanaz maradhat. A játékonként különböző EVAL üzenetekben meg kellene egyeznünk.

