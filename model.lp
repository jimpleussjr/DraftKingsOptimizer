/* Objective function */
max: +21.8 Alvin Kamara +21 Ezekiel Elliott +21 Tom Brady +20.9 Todd Gurley II +20.2 Antonio Brown +20.2 Ryan Fitzpatrick
 +19.5 Michael Thomas +19.5 David Johnson +19.5 Patrick Mahomes +19.4 Aaron Rodgers +19.1 Carson Wentz
 +19 Alex Smith +18.8 Christian McCaffrey +18.7 Cam Newton +18.6 Julio Jones +18.5 Saquon Barkley +18.3 Russell Wilson
 +18.2 Deshaun Watson +18.2 Ben Roethlisberger +18.2 Dalvin Cook +18 Matthew Stafford +17.9 Jimmy Garoppolo
 +17.9 Odell Beckham Jr. +17.9 DeAndre Hopkins +17.4 A.J. Green +17.3 Keenan Allen +17.3 James Conner
 +17.3 Matt Ryan +17.3 Rob Gronkowski +17.2 Drew Brees +17 Kirk Cousins +17 Tevin Coleman +16.6 Andy Dalton
 +16.6 Mitchell Trubisky +16.5 Andrew Luck +16.5 Kareem Hunt +16.5 Ryan Tannehill +16.4 Tyreek Hill +16.3 Leonard Fournette
 +16.1 Jared Goff +16.1 Jordan Howard +15.9 Tyrod Taylor +15.8 T.Y. Hilton +15.7 Mike Evans +15.5 Dak Prescott
 +15.5 Davante Adams +15.3 Adam Thielen +15 Blake Bortles +14.9 Philip Rivers +14.7 Derek Carr +14.6 Lamar Miller
 +14.5 Zach Ertz +14.5 JuJu Smith-Schuster +14.3 Case Keenum +14.2 Golden Tate +14.2 Melvin Gordon III
 +14 Marcus Mariota +13.9 Larry Fitzgerald +13.9 Marvin Jones Jr. +13.8 Amari Cooper +13.8 Sam Bradford
 +13.7 Travis Kelce +13.7 Allen Robinson II +13.7 Kenyan Drake +13.6 Stefon Diggs +13.6 Sam Darnold +13.6 Nelson Agholor
 +13.5 Eli Manning +13.5 Giovani Bernard +13.4 Emmanuel Sanders +13.4 Demaryius Thomas +13.2 Vikings
 +13.2 Chris Thompson +13.1 Joe Flacco +8.7 Jaguars +7.6 Browns +7.4 Texans +6.9 Bears +6.9 Eagles +6.8 Cardinals
 +6.8 Cowboys +6.8 Rams +6.8 Ravens +6.6 Bengals +6.4 Panthers;

/* Constraints */
QB: +Tom Brady +Ryan Fitzpatrick +Patrick Mahomes +Aaron Rodgers +Carson Wentz +Alex Smith +Cam Newton +Russell Wilson
 +Deshaun Watson +Ben Roethlisberger +Matthew Stafford +Jimmy Garoppolo +Matt Ryan +Drew Brees +Kirk Cousins
 +Andy Dalton +Mitchell Trubisky +Andrew Luck +Ryan Tannehill +Jared Goff +Tyrod Taylor +Dak Prescott
 +Blake Bortles +Philip Rivers +Derek Carr +Case Keenum +Marcus Mariota +Sam Bradford +Sam Darnold +Eli Manning
 +Joe Flacco = 1;
RB: +Alvin Kamara +Ezekiel Elliott +Todd Gurley II +David Johnson +Christian McCaffrey +Saquon Barkley +Dalvin Cook
 +James Conner +Tevin Coleman +Kareem Hunt +Leonard Fournette +Jordan Howard +Lamar Miller +Melvin Gordon III
 +Kenyan Drake +Giovani Bernard +Chris Thompson >= 2;
WR: +Antonio Brown +Michael Thomas +Julio Jones +Odell Beckham Jr. +DeAndre Hopkins +A.J. Green +Keenan Allen
 +Tyreek Hill +T.Y. Hilton +Mike Evans +Davante Adams +Adam Thielen +JuJu Smith-Schuster +Golden Tate
 +Larry Fitzgerald +Marvin Jones Jr. +Amari Cooper +Allen Robinson II +Stefon Diggs +Nelson Agholor +Emmanuel Sanders
 +Demaryius Thomas >= 3;
TE: +Alvin Kamara +Ezekiel Elliott +Todd Gurley II +Antonio Brown +Michael Thomas +David Johnson +Christian McCaffrey
 +Julio Jones +Saquon Barkley +Dalvin Cook +Odell Beckham Jr. +DeAndre Hopkins +A.J. Green +Keenan Allen
 +James Conner +Rob Gronkowski +Tevin Coleman +Kareem Hunt +Tyreek Hill +Leonard Fournette +Jordan Howard
 +T.Y. Hilton +Mike Evans +Davante Adams +Adam Thielen +Lamar Miller +Zach Ertz +JuJu Smith-Schuster
 +Golden Tate +Melvin Gordon III +Larry Fitzgerald +Marvin Jones Jr. +Amari Cooper +Travis Kelce +Allen Robinson II
 +Kenyan Drake +Stefon Diggs +Nelson Agholor +Giovani Bernard +Emmanuel Sanders +Demaryius Thomas +Chris Thompson = 7;
Flex: +Rob Gronkowski +Zach Ertz +Travis Kelce >= 1;
DST: +Vikings +Jaguars +Browns +Texans +Bears +Eagles +Cardinals +Cowboys +Rams +Ravens +Bengals +Panthers = 1;
Salary: +9500 Alvin Kamara +7700 Ezekiel Elliott +6900 Tom Brady +9200 Todd Gurley II +8800 Antonio Brown +6000 Ryan Fitzpatrick
 +8900 Michael Thomas +7200 David Johnson +7000 Patrick Mahomes +7100 Aaron Rodgers +6200 Carson Wentz
 +5800 Alex Smith +7800 Christian McCaffrey +6000 Cam Newton +7900 Julio Jones +7600 Saquon Barkley +6000 Russell Wilson
 +6100 Deshaun Watson +6300 Ben Roethlisberger +7100 Dalvin Cook +5900 Matthew Stafford +6500 Jimmy Garoppolo
 +8200 Odell Beckham Jr. +8300 DeAndre Hopkins +7500 A.J. Green +7300 Keenan Allen +7500 James Conner
 +5700 Matt Ryan +7400 Rob Gronkowski +6400 Drew Brees +6800 Kirk Cousins +6400 Tevin Coleman +5700 Andy Dalton
 +5500 Mitchell Trubisky +5600 Andrew Luck +6000 Kareem Hunt +5300 Ryan Tannehill +8500 Tyreek Hill +6900 Leonard Fournette
 +5900 Jared Goff +6500 Jordan Howard +5700 Tyrod Taylor +6700 T.Y. Hilton +7800 Mike Evans +5100 Dak Prescott
 +7600 Davante Adams +7800 Adam Thielen +5600 Blake Bortles +5800 Philip Rivers +5400 Derek Carr +5000 Lamar Miller
 +6800 Zach Ertz +7200 JuJu Smith-Schuster +5200 Case Keenum +6800 Golden Tate +7400 Melvin Gordon III
 +4900 Marcus Mariota +5400 Larry Fitzgerald +5600 Marvin Jones Jr. +6600 Amari Cooper +4500 Sam Bradford
 +6700 Travis Kelce +5400 Allen Robinson II +5600 Kenyan Drake +7700 Stefon Diggs +5100 Sam Darnold +6100 Nelson Agholor
 +5200 Eli Manning +5900 Giovani Bernard +6400 Emmanuel Sanders +5200 Demaryius Thomas +4300 Vikings
 +6300 Chris Thompson +5400 Joe Flacco +4000 Jaguars +3000 Browns +2800 Texans +3500 Bears +3000 Eagles
 +2400 Cardinals +2200 Cowboys +3100 Rams +2800 Ravens +2500 Bengals +2700 Panthers <= 50000;

/* Variable bounds */
Alvin Kamara <= 1;
Ezekiel Elliott <= 1;
Tom Brady <= 1;
Todd Gurley II <= 1;
Antonio Brown <= 1;
Ryan Fitzpatrick <= 1;
Michael Thomas <= 1;
David Johnson <= 1;
Patrick Mahomes <= 1;
Aaron Rodgers <= 1;
Carson Wentz <= 1;
Alex Smith <= 1;
Christian McCaffrey <= 1;
Cam Newton <= 1;
Julio Jones <= 1;
Saquon Barkley <= 1;
Russell Wilson <= 1;
Deshaun Watson <= 1;
Ben Roethlisberger <= 1;
Dalvin Cook <= 1;
Matthew Stafford <= 1;
Jimmy Garoppolo <= 1;
Odell Beckham Jr. <= 1;
DeAndre Hopkins <= 1;
A.J. Green <= 1;
Keenan Allen <= 1;
James Conner <= 1;
Matt Ryan <= 1;
Rob Gronkowski <= 1;
Drew Brees <= 1;
Kirk Cousins <= 1;
Tevin Coleman <= 1;
Andy Dalton <= 1;
Mitchell Trubisky <= 1;
Andrew Luck <= 1;
Kareem Hunt <= 1;
Ryan Tannehill <= 1;
Tyreek Hill <= 1;
Leonard Fournette <= 1;
Jared Goff <= 1;
Jordan Howard <= 1;
Tyrod Taylor <= 1;
T.Y. Hilton <= 1;
Mike Evans <= 1;
Dak Prescott <= 1;
Davante Adams <= 1;
Adam Thielen <= 1;
Blake Bortles <= 1;
Philip Rivers <= 1;
Derek Carr <= 1;
Lamar Miller <= 1;
Zach Ertz <= 1;
JuJu Smith-Schuster <= 1;
Case Keenum <= 1;
Golden Tate <= 1;
Melvin Gordon III <= 1;
Marcus Mariota <= 1;
Larry Fitzgerald <= 1;
Marvin Jones Jr. <= 1;
Amari Cooper <= 1;
Sam Bradford <= 1;
Travis Kelce <= 1;
Allen Robinson II <= 1;
Kenyan Drake <= 1;
Stefon Diggs <= 1;
Sam Darnold <= 1;
Nelson Agholor <= 1;
Eli Manning <= 1;
Giovani Bernard <= 1;
Emmanuel Sanders <= 1;
Demaryius Thomas <= 1;
Vikings <= 1;
Chris Thompson <= 1;
Joe Flacco <= 1;
Jaguars <= 1;
Browns <= 1;
Texans <= 1;
Bears <= 1;
Eagles <= 1;
Cardinals <= 1;
Cowboys <= 1;
Rams <= 1;
Ravens <= 1;
Bengals <= 1;
Panthers <= 1;

/* Integer definitions */
int Alvin Kamara,Ezekiel Elliott,Tom Brady,Todd Gurley II,Antonio Brown,Ryan Fitzpatrick,Michael Thomas,David Johnson,Patrick Mahomes,Aaron Rodgers,Carson Wentz,Alex Smith,Christian McCaffrey,Cam Newton,Julio Jones,Saquon Barkley,Russell Wilson,Deshaun Watson,Ben Roethlisberger,Dalvin Cook,Matthew Stafford,Jimmy Garoppolo,Odell Beckham Jr.,DeAndre Hopkins,A.J. Green,Keenan Allen,James Conner,Matt Ryan,Rob Gronkowski,Drew Brees,Kirk Cousins,Tevin Coleman,Andy Dalton,Mitchell Trubisky,Andrew Luck,Kareem Hunt,Ryan Tannehill,Tyreek Hill,Leonard Fournette,Jared Goff,Jordan Howard,Tyrod Taylor,T.Y. Hilton,Mike Evans,Dak Prescott,Davante Adams,Adam Thielen,Blake Bortles,Philip Rivers,Derek Carr,Lamar Miller,Zach Ertz,JuJu Smith-Schuster,Case Keenum,Golden Tate,Melvin Gordon III,Marcus Mariota,Larry Fitzgerald,Marvin Jones Jr.,Amari Cooper,Sam Bradford,Travis Kelce,Allen Robinson II,Kenyan Drake,Stefon Diggs,Sam Darnold,Nelson Agholor,Eli Manning,Giovani Bernard,Emmanuel Sanders,Demaryius Thomas,Vikings,Chris Thompson,Joe Flacco,Jaguars,Browns,Texans,Bears,Eagles,Cardinals,Cowboys,Rams,Ravens,Bengals,Panthers;
