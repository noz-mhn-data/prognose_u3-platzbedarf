# Program: Prognose_U3-Platzbedarf_NOZ.R ----
#
# Prognostiziert den Platzbedarf an U3-Kinderbetreuung bis 2025
# für die Regionen Flaeche_West (FW), Flaeche_Ost (FO) und Stadtstaaten (ST).
#
# Benutzt die Methodik von DJI/akjstat in der Publikation
# "Plätze. Personal. Finanzen – der Kita-Ausbau geht weiter
# Zukunftsszenarien zur Kindertages- und Grundschulbetreuung in Deutschland
# Version 2-2017" (PPF_2017), siehe Datei 
# "Prognose_Platzbedarf_bis_2025_Rauschenbach_Schilling_Meiner-Teubner_Plaetze._Personal._Finanzen.pdf" 
# unter https://www.dji.de/fileadmin/user_upload/bibs2017/rauschenbach_schilling_plaetze_personal_finanzen.pdf
#
# DIESE NOZ- Prognose verwendet folgende Annahmen:
# - Elternwunsch nach U3-Betreuung steigt dem Trend folgend
# - Jahrgangsstärken entsprechend aktuellsten Zahlen.
# - Langfristige Steigerung der Jahrgangsstärken über die 
#   Geburtenraten in der Destatis-Prognose (KO2A, s.u.) hinaus um
#   FW: +2,8%    FO: +3,8%    ST: +2,0%
#   Grund: Geburtenraten sind gestiegen
#   Verfahren im Detail siehe Abschnitt "IIIc: Jahrgänge für NOZ-Prognose bestücken"
#
# Grundlage für die Jahrgangsstärkenberechnungen sind unter der Abkürzung KO2A
# die Ergebnisse der 13. koordinierten Bevölkerungsvorausberechnung des Statistischen
# Bundesamtes, Aktualisierte Rechnung auf Basis 2015, Variante 2-A
# siehe: https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsvorausberechnung/Publikationen/Downloads-Vorausberechnung/bevoelkerung-bundeslaender-2060-aktualisiert-5124207179004.html

# ********** ACHTUNG: Arvbeitsverzeichnis setzen! **********
# Damit sie diesen Code auf Ihrem Computer ablaufen lassen können,
# entkommentieren Sie die folgende Zeile und tragen Sie den Pfad
# zu dem Datei-Ordner ein, in der auf Ihrem Rechner die Datei 
# "Prognose_U3-Platzbedarf_NOZ.R" liegt. In diesem Ordner mus auch 
# ein Unterordner namens "data" mit sämtlichen Daten des entsprechenden 
# NOZ GitHub-Orners liegen

#setwd("ERSETZEN_DURCH_KOMPLETTEN_PFAD_ZU_IHREM_ORDNER")

# ---- Optionen & Pakete ----
options(scipen=100, OutDec=",")  # Keine wissenschaftlichen Zahlen, immer normale Kommazahlen
par(xpd=FALSE)                   

# Pakete laden
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(readr)

# ---- 0a. Daten laden & vorbereiten ---- 

# Kita-Plätze Betreuungsquoten und BEdarfsquoten (bis 2017) laden
bedarf <- read_excel("data/KiBe_Quoten_&_Bedarfe_2018.xlsx",range = "A1:H238", 
						    col_types = c("numeric","text", "text", "text", "text", 
						    		    "numeric", "numeric", "numeric"))

# U3-Jahrgänge aus amtlicher Fortschreibung laden:

# ACHTUNG: Alle Werte zum 31.12. des Jahres!
# Da KitaPlätze & -quoten je zum 01.03. erhoben werden,
# gilt für sie die Jahrgangsstärke des Vorjahres. Beispiel:
# Kita-Werte aus 2015 gehören zu Jahrgangstärken aus 2014.
# Wir nutzen die Kita-Erhebung zur Nummerierung der Jahre.
# Die Jaherszahlen der Fortschreibung werden angepasst:
# Es wir also je ein Jahr hinzuaddiert.
# Es wird also jeweisl zum 1.3. eines Jahres mit der Kinderanzahl von drei Monate zuvor gerechnet.
# Dies entspricht den Konventionen von Destatis, DJI und akjstat.

fs_u3_load <- read_excel("data/Fortschreibung_U3_Bundeslaender_2005_2017.xlsx", 
											 sheet = "12411-0012", skip = 4)
# Bundesländer mit AGS dazu laden:
bundeslaender <- read_delim("data/bundeslaender.tsv", "\t",          
					   escape_double = FALSE, trim_ws = TRUE)
# Bundesländer nach Regionstyp ausgeben: 
# bundeslaender %>% arrange(rs) %>% filter(region=="FW") %>% pull(land) %>% paste(collapse=", ")

# Destatis-Datei bereinigen:
fs_u3_laender <- fs_u3_load %>%
	rename( land = 1, kinder_u1 = "unter 1 Jahr",
		   kinder_1_bis_u2 = "1-Jährige", kinder_2_bis_u3 = "2-Jährige" ) %>%
	select (-Insgesamt) %>%
	mutate(jahr = ifelse(str_sub(land, 1, 2) == '31',         # Wenn Datum in erster Spalte, dann auslesen und in eigene "jahr"-Spalte schreiben
					 as.numeric(str_sub(land, -4)) + 1,   # Jahresnummer der Jahrgangsstärken der Kitajahr-Zählung anpassen
					 NA)) %>%
	fill(jahr) %>%                                # Fehlende Jahreszahlen mit zuletzt vorhandener auffüllen (tidyr)
	filter(!is.na(kinder_u1)) %>%                 # NA-Zeilen (in denen nur das Datum stand) rauswerfen
	mutate(kinder_u3 = kinder_u1 + kinder_1_bis_u2 + kinder_2_bis_u3	) %>%
	left_join(bundeslaender) %>%
	select(jahr, rs, land, abkuerzung, typ, region,
		  kinder_u1, kinder_1_bis_u2, kinder_2_bis_u3, kinder_u3)

# Summen für Deutschland bilden (für jedes Jahr):
fs_u3_de <- fs_u3_laender %>%
	group_by(jahr) %>%
	summarise_if(is.numeric, sum) %>%
	mutate(rs ="00", land ="Deutschland", abkuerzung ="DE", 
		  typ = "Bund", region = "DE")

# Data.Frame mit Werten für Länder & Deutschland zusammen:
fs_u3 <- bind_rows(fs_u3_laender, fs_u3_de)

# Änderunf & Lücke von Kita-Plätzen berechnen (BUND):
bedarf_bund <- bedarf %>%
	filter(typ=="Bund") %>%
	mutate(aenderung_plaetze = plaetze-lag(plaetze)) %>%
	mutate(aenderung_quote = quote-lag(quote)) %>%
	left_join(select(fs_u3_de, jahr, typ, kinder_u3)) %>%
	mutate(bedarf_plaetze = round(kinder_u3 * bedarfsquote_u3/100)) %>%
	mutate(luecke_plaetze = round(bedarf_plaetze - plaetze))

# ---- 0b. Test Berechnungsmethode Bedarfstrend für ganz Deutschland ---- 

##### Durchschnittliche Änderung des Bedarfs berechnen (Bund!)

year_start <- 2012
year_end <- 2017

## 1. Aus Differenz der Quoten

duration <- year_end - year_start
bedarf_start <- filter(bedarf_bund, jahr==year_start)$bedarfsquote_u3  # -> 39,4% Bedarf in 2012
bedarf_end <- filter(bedarf_bund, jahr==year_end)$bedarfsquote_u3      # -> 45,2% Bedarf in 2017 
bedarf_aenderung <- bedarf_end - bedarf_start          # -> Bedarf nahm von 2012 bis 2017 um +5,8 %-Punkte zu

rate_differenz <- (bedarf_aenderung)/duration    # -> pro Jahr von 2012 bis 2017 durchschnittl. Zunahme des Bedarfs um 1,16 %-Punkte

## 2. Aus Linearer Regression

x <- bedarf_bund %>% filter(jahr %in% year_start:year_end) %>% pull(jahr)
y <- bedarf_bund %>% filter(jahr %in% year_start:year_end) %>% pull(bedarfsquote_u3)

regression <- lm(y ~ x)

summary(regression)
rate_regression <- as.numeric(regression$coefficients[2])   # -> Durchschnittl. Zunahme 1,24%-Punkte 
# abline(regression, col="red")

# Ergebnis: Als Grundlage für die Extrapolation der Bedarfe für die NOZ-Prognose sollte
# die lineare Regression der KiBS-Daten ab 2012 gewählt werden.
# Gründe:
# - Einheitliche Erhebung seit 2012
# - Bundeslandgenau -> Bedarfsquoten für die Regionen Flaeche_West, Flaeche_Ost und Stadtstaaten 
#   lassen sich durch Gewichtung berechnen
# - Regression gleicht Schwankungen aus


# I. Berechnung der Bedarfsquoten für 3 Regionen FW, FO, ST für den gesamten Zeitraum ----

# ---- Ia: Berechnung Bedarfsquoten 2012 - 2017 (KiBS-Jahre)  ----
# Werte für die 3 Regionen durch Gewichtung der KiBS Bundesländer-Bedarfsquoten 
# mit den U3-Jahrgangsstärken der Bundesländer aus der amtlichen Fortschreibung

# Fortschreibung der Jahrgangsstärken (fs_u3) und KiBS-Bedarfsquoten (bedarf) mergen:
fortschreibung_bedarfe_u3 <- fs_u3 %>%
	filter(jahr >= 2012, region != "DE") %>%
	inner_join(select(bedarf, jahr, abkuerzung, quote, bedarfsquote_u3), by = c("jahr", "abkuerzung"))
	
# Bedarfsquoten und Fortschreibung für 3 Regionen FW, FW, ST:
# Berechnung Quoten und der Bedarfsquoten als gewichteter Mittelwert (weighted.mean) 
# der Quoten in den Bundesländern, aus denen die Region besteht 
# (Zuordnung siehe Spalte "region" in dataframe "bundeslaender").
# Die Gewichte sind die Anzahlen der U3-Kinder pro Bundesland.

fortschreibung_bedarfe_u3_regionen <- fortschreibung_bedarfe_u3 %>%
	group_by(jahr, region) %>%
	summarize(quote_u3 = round(weighted.mean(quote, kinder_u3),2),
			bedarfsquote_u3 = round(weighted.mean(bedarfsquote_u3, kinder_u3),2),
			kinder_u1 = sum(kinder_u1), 
			kinder_1_bis_u2  = sum(kinder_1_bis_u2),
			kinder_2_bis_u3  = sum(kinder_2_bis_u3),
			kinder_u3 = sum(kinder_u3))  # Daten bis 2018, aber Bedarfsquote NUR BIS 2017

# Plot der Quoten und Bedarfe für die drei Regionen:
col1 <- "red"; col2 <- "green"; col3 <- "blue";  
plot(x=fortschreibung_bedarfe_u3_regionen$jahr, y=fortschreibung_bedarfe_u3_regionen$bedarfsquote_u3, 
	pch = 19, ylim = c(0,65), 
	xlab ='', ylab='Bedarfsquoten U3 in %', 
	las=1, bty = 'n', col = c(col1, col2, col3))
points(x=fortschreibung_bedarfe_u3_regionen$jahr, y=fortschreibung_bedarfe_u3_regionen$quote_u3, 
	pch = 3, col = c(col1, col2, col3))

text(2012, 58, "Fläche Ost", pos=4, col = col1)
text(2013, 48, "Stadtstaaten", pos=4, col = col3)
text(2014, 32, "Fläche West", pos=4, col = col2)

# ---- Ib: Lineare Regressionen zur Berechnung der jährl. Zuwachsraten pro Region für Extrapolation ----

# Funktion zur Berechnung der Ergebnisse eines linearen Modells
steigung <- function(model){
	# print(summary(model))
	coefficient <- as.numeric(model$coefficients[2])
	p_value <- summary(model)$coefficients[2,4] 
	R2 <- summary(model)$r.squared
	
	return(c(coefficient, p_value, R2))
}

# Fläche West:
df <- fortschreibung_bedarfe_u3_regionen %>%
	filter(region == "FW" & jahr!=2018)
x <- df$jahr
y <- df$bedarfsquote_u3
model_Flaeche_West <- lm(y ~ x)

# Fläche Ost:
df <- fortschreibung_bedarfe_u3_regionen %>%
	filter(region == "FO" & jahr!=2018)
x <- df$jahr
y <- df$bedarfsquote_u3
model_Flaeche_Ost <- lm(y ~ x)

# Stadtstaaten:
df <- fortschreibung_bedarfe_u3_regionen %>%
	filter(region == "ST" & jahr!=2018)
x <- df$jahr
y <- df$bedarfsquote_u3
model_Stadtstaaten <- lm(y ~ x)


trend_regionen <- data.frame(region = c("FW", "FO", "ST"),
							  steigung = c(steigung(model_Flaeche_West)[1], steigung(model_Flaeche_Ost)[1], steigung(model_Stadtstaaten)[1]),
							  p_value = c(steigung(model_Flaeche_West)[2], steigung(model_Flaeche_Ost)[2], steigung(model_Stadtstaaten)[2]),
							  R2 = c(steigung(model_Flaeche_West)[3], steigung(model_Flaeche_Ost)[3], steigung(model_Stadtstaaten)[3]))

# Trendgeraden in Plot mit Bedarfsquoten der Regionen eintragen:
abline(model_Flaeche_West, col=col2)
abline(model_Flaeche_Ost, col=col1)
abline(model_Stadtstaaten, col=col3)


# ---- Ic: Extrapolation der Bedarfsquoten ----

x.new <- data.frame(x = 2018:2025)  # Jahreswerte als unabh. Variable x für Regressionen

# Für alle drei Regionen die Bedarfsquoten projizieren (2018 bis 2025):
bedarfe_u3_regionen_prognose_FW <- data.frame(jahr = x.new$x,
									 region = rep("FW", nrow(x.new)),
									 bedarfsquote_u3 = as.vector(round(predict(model_Flaeche_West, x.new),2)),
									 stringsAsFactors = FALSE)
bedarfe_u3_regionen_prognose_FO <- data.frame(jahr = x.new$x,
									 region = rep("FO", nrow(x.new)),
									 bedarfsquote_u3 = as.vector(round(predict(model_Flaeche_Ost, x.new),2)),
									 stringsAsFactors = FALSE)
bedarfe_u3_regionen_prognose_ST <- data.frame(jahr = x.new$x,
									 region = rep("ST", nrow(x.new)),
									 bedarfsquote_u3 = as.vector(round(predict(model_Stadtstaaten, x.new),2)),
									 stringsAsFactors = FALSE)
bedarfe_u3_regionen_prognose <- bind_rows(bedarfe_u3_regionen_prognose_FW, bedarfe_u3_regionen_prognose_FO,
								  bedarfe_u3_regionen_prognose_ST)

bedarfsquoten <- fortschreibung_bedarfe_u3_regionen %>%
	filter(jahr != 2018) %>%
	select(jahr, region, bedarfsquote_u3) %>%
	bind_rows(bedarfe_u3_regionen_prognose) %>%
	arrange(region, jahr)


# II. Berechnung in PPF_2017 nachvollziehen ----
# Nachzuvollziehende Werte siehe "Szenario 2: Zusätzlicher Platzbedarf aufgrund demografischer Veränderungen 
# und nicht erfüllter Elternwünsche", in PPF_2017 Tabelle 12 S. 18f.


# ---- IIa: PPF-Basisdaten laden & Platzzahlen für DE checken ----
# Ziel:
# Maximaler Bedarf an zusätzlichen Betreuungsplätzen 
# in ganz Deutschland im Vergleich zu 2016 (zu unterschiedlichen Zeitpunkten)

# PPF-Ergebnisse laden (Fläche West, Fläche Ost & Stadtstaaten):
ppf <- read_excel("data/Ergebnisse_PPF_2017.xlsx", sheet = "Daten",
			   col_types = c("numeric", "text", "text", rep("numeric", 10))) %>%
	mutate(region = recode(region, 
					   "Flaeche_West" = "FW",
					   "Flaeche_Ost" = "FO",
					   "Stadtstaaten" = "ST",
					   "Deutschland" = "DE"))

# Normale Jahreswerte und Maximal-Zeile trennen:
ppf_werte <- ppf  %>% 
	filter(typ != "Maximal_PPF_2017")
	
ppf_max_pdf <- ppf %>% filter(typ == "Maximal_PPF_2017")  # Maximalwert für 2025 und Jahre dvor aus letzter Tabellenzeile im PDF S. 19

# Summen für Gesamtdeutschland ausrechnen:
ppf_werte_de_rechts <- ppf_werte %>% 
	group_by(jahr) %>% 
	summarize_at(vars(plaetze_u3:diff_6.5_bis_10.5_gts), sum)

ppf_werte_de_links <- ppf_werte %>% 
	filter(region=="FW") %>%
	mutate(region = "DE") %>%
	select(jahr:typ)

ppf_werte_de <- ppf_werte_de_links %>% 
	inner_join(ppf_werte_de_rechts, by = c("jahr"="jahr"))

# Check Maximalbedarf bis 2025:

ppf_max_check_2025 <- ppf_werte %>%
	group_by(region) %>%
	summarize_if(is.numeric, max) %>%    # Erst Maximalwert für alle 3 Regionen bestimmen...
	summarize_if(is.numeric, sum)        # ... dann zusammenzählen

# Vergleiche zu 307.165 Plätzen in PPF_2017

# ---- IIb: Check Investitionskosten f. Kitas ----

kosten_kita_neubau_pp <- 36000     # Kosten pro Platz Vgl. PPF Tabelle 27, S. 41
kosten_kita_anbau_pp <- 18000

investitionen_kita_check <- 0.5 * ppf_max_check_2025$diff_2016_u3_kita * kosten_kita_neubau_pp +
	0.5 * ppf_max_check_2025$diff_2016_u3_kita * kosten_kita_anbau_pp # Je Hälfte der Plätze durch Neu- bzw- Anbau

# Vergleich PPF_2017: 6,936 Mrd. Euro (Summe aus Tabelle 29 S. 42)

# ---- IIc: PPF-Platzzahlen aus Jahrgangsprognosen der aktualisierten KO berechnen (also PPF_2017 nachrechnen).  ----
# Dafür: Bedarfe für Flächenländer Ost/West und Stadtstaaten hernehmen und Berechnung per Gewichtung aus den Quotenbedarfen 
# der einzelnen Länder checken

# Bedarfe 2016 für die drei Regionen siehe PPF_2017 Tab. 3 S.12: 
# Vermutung: Prognose in PPF_2017 setzt für alle Jahre die 
# Quotenbedarfe aus 2016 an:
quoten_interim <- data.frame(region = c("FW", "FO", "ST", "DE" ), 
					    bedarfsquote_u3 = c(42.6, 59.5, 54.4, 46.0), stringsAsFactors = FALSE)

#... Ddas ganz als Dataframe fpür alle Prognosejahre (inkl. 2016) und Regionen:
ppf_check_bedarfsquoten <- ppf_werte %>%
	select(c("jahr", "region")) %>%
	inner_join(quoten_interim, c("region"="region"))

# Lade Anzahl der U3-Kinder pro Prognosejahr,
# siehe Tabelle 1 S. 8 in PPF_2017,
# entspricht aktualisierter KO Destatis (KO2A, Basis 31.12.2015).
# ACHTUNG: PPF_2017 korrigiert die Jahreszahl NICHT gemäß Destatis-Gepflogenheit.
# Dem Erhebungsjahr 2017 für die Bedarfe (Erhebungszeitraum Januar bis Oktober)
# werden also die Kinderzahlen vom Jahresende 2017 zugeordnet.

ppf_jahrgaenge_all <- read_excel("data/Jahrgaenge_PPF_2017.xlsx", sheet = "Daten",
			   col_types = c("numeric", "text", rep("numeric", 4))) %>%
	mutate(region = recode(region, 
					   "Flaeche_West" = "FW",
					   "Flaeche_Ost" = "FO",
					   "Stadtstaaten" = "ST",
					   "Deutschland" = "DE"))

ppf_jahrgaenge <- filter(ppf_jahrgaenge_all, region != "DE")

# Bedarfsquoten dazu joinen:
ppf_check_interim <- ppf_check_bedarfsquoten %>%
	inner_join(select(ppf_jahrgaenge, c("jahr", "region", "kinder_u3")))

# Platzbedarf pro Prognosejahr für 3 Regionen berechnen: Anzahl Kinder * Bedarfsquote
ppf_check_prognose <- ppf_check_interim %>%
	mutate(bedarf_noz = kinder_u3 * bedarfsquote_u3/100) %>%        # Nachgerechnete Platzzahlen
	inner_join(select(ppf_werte, c("jahr", "region", "plaetze_u3"))) %>%
	rename(plaetze_u3_ppf = plaetze_u3) %>%
	mutate(bedarfsquote_ppf_RevEng = round(plaetze_u3_ppf/kinder_u3*100,2)) # Rückgerechnete PPF-Quote (deren wahre Annahme)

# Aus PPF_2017-Werten der Regionen deren Platzbedarf für DE berechnen
# und die entsprechende DE-Annahme für die Bedarfsquote rückrechnen
ppf_RevEng_de <- ppf_check_prognose %>%
	select(jahr, region, plaetze_u3_ppf, kinder_u3) %>%
	group_by(jahr) %>%
	summarize(plaetze_u3_ppf = sum(plaetze_u3_ppf), kinder_u3 = sum(kinder_u3)) %>%   # Absolute Zahlen über 3 Regionen summieren
	mutate(bedarfsquote_ppf_RevEng = round(plaetze_u3_ppf/kinder_u3*100,2)) %>%       # Rückgerechnete PPF-Quote
	mutate(region = "DE")
	

# Ergebnis:
# PPF_2017 setzt NICHT sofort für alle Jahre die Quotenbedarfe aus 2016 an (bedarfsquote_u3 = c(42.6, 59.5, 54.4, 46.0)),
# sondern erhöht die Quote von 2017 bis 2025 kontinuierlich um fixe Prozentpunkte, bis sie 2025 fast den 2016er-Bedarfen entsprechen.
# Flaeche_West: 2017 = 29,4; jährl. Steigerung = 0,8; ab 2024 = 42,0 
# Flaeche_Ost: 2017 = 54,75 ; jährl. Steigerung = 0,9 ; ab 2022 = 59,25
# Stadtstaaten: 2017 = 44,35; jährl. Steigerung = 1,3 ; ab 2024 = 54,45
# 
# -> Das ist die fortgeschriebene Inanspruchnahme aus dem U3-Ausbau 2010 bis 2016, NICHT aber die Bedarfe.
# -> Siehe PPF Tabellen 4 & 5 S. 12
# -> ERGO: Als Bedarf gilt das, was bisher möglich war; wahrer Bedarf kommt nicht vor 

# III: Eigene Prognose "model_I" berechnen ----
# Dafür linear steigende Bedarfe gemäß linearer Regression aus gewichteten
# Bedarfen für die drei Regionen ansetzen und Jahrgangsstärken aus aktualisierter KO ersetzen durch 
# realistische Werte


# ---- IIIa Jahrgangsstärken der aktualisierten KO 2A für alle drei Regionen einlesen und bereinigen ----

# Daten für westliche Flächenstaten aus Destatis-Datei laden und bereinigen:
# ACHTUNG: Alle Bevökerungszahlen wieder zum 31.12.
# -> Müssen wieder die Zahlen vom Vorjahr nehmen.
# Ergo: Jahreszahl -> Jahreszahl + 1

KO2A_FW <- read_excel("data/BevoelkerungBundeslaender2060_Aktualisiert_5124207179005.xlsx", 
					sheet = "Variante 2-A FLW EJ", col_names = FALSE, skip = 16,
					col_types = c("numeric", "text", rep("numeric", 102)))
colnames(KO2A_FW) <- c("jahr", "geschlecht", "gesamt", paste(0:99, "_bis_u",1:100, "_KO", sep=""), "100+")

jahrgaenge_KO2A_FW <- KO2A_FW %>% 
	fill(jahr) %>%
	filter(geschlecht == "i", jahr %in% 2015:2024) %>%
	mutate(region = "FW") 

# Östliche Flächenstaten aus Destatis-Datei laden und bereinigen:
KO2A_FO <- read_excel("data/BevoelkerungBundeslaender2060_Aktualisiert_5124207179005.xlsx", 
				  sheet = "Variante 2-A 2 FLO EJ", col_names = FALSE, skip = 16,
				  col_types = c("numeric", "text", rep("numeric", 102)))

colnames(KO2A_FO) <- c("jahr", "geschlecht", "gesamt", 
				   paste(0:99, "_bis_u",1:100, "_KO", sep=""), "100+")		# Spalten umbenennen 

jahrgaenge_KO2A_FO <- KO2A_FO %>% 
	fill(jahr) %>% 								# Jahreszahlen vervollständigen
	filter(geschlecht == "i", jahr %in% 2015:2024) %>%	# Nur Zahlen fpr Jungen & Mädchen zusammen behalten
	mutate(region = "FO") 							# Region eintragen

# Stadtstaten aus Destatis-Datei laden und bereinigen:
KO2A_ST <- read_excel("data/BevoelkerungBundeslaender2060_Aktualisiert_5124207179005.xlsx", 
				  sheet = "Variante 2-A StSt EJ", col_names = FALSE, skip = 16,
				  col_types = c("numeric", "text", rep("numeric", 102)))

colnames(KO2A_ST) <- c("jahr", "geschlecht", "gesamt", paste(0:99, "_bis_u",1:100, "_KO", sep=""), "100+")

jahrgaenge_KO2A_ST <- KO2A_ST %>% 
	fill(jahr) %>%
	filter(geschlecht == "i", jahr %in% 2015:2024) %>%
	mutate(region = "ST") 

# Alle Regionen in einem Dataframe zusammenfassen: 

jahrgaenge_KO2A <- bind_rows(jahrgaenge_KO2A_FW, jahrgaenge_KO2A_FO, jahrgaenge_KO2A_ST) %>%	# Alle Regionen hintereinanderpacken
	select(jahr, region, "0_bis_u1_KO", "1_bis_u2_KO", "2_bis_u3_KO") %>%					# Nur unsere Jahre & U3- Altersstufen behalten 
	mutate(u3_KO = `0_bis_u1_KO` + `1_bis_u2_KO` + `2_bis_u3_KO`) %>%					# U3-Summe berechnen
	mutate_at(vars(`0_bis_u1_KO`, `1_bis_u2_KO`, `2_bis_u3_KO`, `u3_KO`), list(~.*1000)) %>%	# Alle Jahrgangszahlen *1000 (Standen in Destatis-Datei als Tausender)
	mutate(jahr = jahr + 1)                                      # Jahreszählung an Kitajahre anpassen
	
# ---- IIIb: KO2A-Jahrgänge mit echten Daten für 2016 und 2017 vergleichen ----

jahrgaenge_vergleich <- jahrgaenge_KO2A %>%
	inner_join(fortschreibung_bedarfe_u3_regionen, by = c("jahr", "region")) %>%
	mutate(diff_u1_absolut = kinder_u1 - `0_bis_u1_KO`, diff_u1_proz = round(diff_u1_absolut/`0_bis_u1_KO`*100,1)) %>%
	mutate(diff_1_bis_u2_absolut = kinder_1_bis_u2 - `1_bis_u2_KO`, diff_1_bis_u2_proz = round(diff_1_bis_u2_absolut/`1_bis_u2_KO`*100,1)) %>%
	mutate(diff_2_bis_u3_absolut = kinder_2_bis_u3 - `2_bis_u3_KO`, diff_2_bis_u3_proz = round(diff_2_bis_u3_absolut/`2_bis_u3_KO`*100,1)) %>%
	mutate(diff_u3_absolut = kinder_u3 - u3_KO, diff_u3_proz = round(diff_u3_absolut/u3_KO*100,1))

# ---- IIIc: Jahrgänge für NOZ-Prognose bestücken (Dataframe jahrgaenge_noz) ----

# Annahmen:
# - IST-Werte für alle Altersjahre für 2016 & 2017 übernehmen
# - Angefangene Jahrgänge von 2016/17 bis 2019 ein Jahr älter weiterführen 
#   (absolute Differenz zu KO2A übernehmen und hinzuaddieren)
# - für alle anderen Altersjahre prozentuale Zuwächse delta_FW, delta_FO und 
#   delta_ST benutzen (für die 3 Regionen)
# - Dann je Region und Jahr U3-Stärken berechnen.

jahrgaenge_noz1 <- jahrgaenge_KO2A %>%									# Ausgangspunkt: Jahrgänge der aktualisierten Destatis-Prognose KO2A
	mutate(`0_bis_u1_noz` = NA, `1_bis_u2_noz` = NA, `2_bis_u3_noz` = NA,
		  kinder_u3_noz = NA) %>%									# Spalten für NOZ-Werte anlegen
	left_join(select(fortschreibung_bedarfe_u3_regionen, jahr, region, 
				  kinder_u1_FS = kinder_u1, 
				  kinder_1_bis_u2_FS = kinder_1_bis_u2, 
				  kinder_2_bis_u3_FS = kinder_2_bis_u3, 
				  kinder_u3_FS = kinder_u3), 
				  by = c("jahr", "region"))	%>%						# Reale Jahrgangswerte für 2016 und 2017 aus Fortscheibung anfügen
	mutate(`0_bis_u1_noz` = kinder_u1_FS, 
		  `1_bis_u2_noz` = kinder_1_bis_u2_FS, 
		  `2_bis_u3_noz` = kinder_2_bis_u3_FS)			# Reale Werte für 2016/17 übernehmen (Rest ist NA)

jahrgaenge_noz2 <- jahrgaenge_noz1 %>%
	left_join(select(jahrgaenge_vergleich,
				  jahr, region, ends_with("absolut"),
				  -diff_u3_absolut), 
			by = c("jahr", "region")) %>%
	mutate(`1_bis_u2_noz` = ifelse(jahr == 2018, 
							 `1_bis_u2_KO` + lag(diff_u1_absolut), 
							 `1_bis_u2_noz`)) %>%	# Zähle für 1- bis u2-Jährige 2018 den Überschuss der u1-Jährigen aus 2017 hinzu
	mutate(`2_bis_u3_noz` = ifelse(jahr == 2018, 
						 	`2_bis_u3_KO` + lag(diff_1_bis_u2_absolut), 
						 	`2_bis_u3_noz`)) %>%	# Zähle für 2- bis u3-Jährige 2018 den Überschuss der 1- bis u2-Jährigen aus 2017 hinzu
	mutate(`2_bis_u3_noz` = ifelse(jahr == 2019, 
							 `2_bis_u3_KO` + (lag(`1_bis_u2_noz`) - lag(`1_bis_u2_KO`)), 
							 `2_bis_u3_noz`))	# Zähle für 2- bis u3-Jährige 2019 den Überschuss der 1- bis u2-Jährigen aus 2018 hinzu
	
# Deltas (=Wachstum der Jahrgangsgrößen über Destatis-Prognose KO2A) festlegen:
# Beispiel: delta = 0.01 entspräche einem Zuwachs von 1 Prozent mehr als Destatis KO2A
# Begründungg für Zuwachs von 2%:
# KOA2 geht ab 2015 von Geburtenrate 1,50 Kinder pro Frau aus
# Inzwischen gibt es aber neuere Daten aus realer Entwicklung:
# Jahre: 2015-2016-2017  --> %-Zuwachs von Geburtenrate 1,50 im Jahr 2015 auf 2017er-Wert
# West: 1,50-1,60-1,58   --> +5,3%
# Ost:  1,56-1,64,-1,61  --> +7,3%  (+3,2% real)
# DE:   1,50-1,59-1,57   --> +4,7%
# Diese %-Zahlen wären als deltas anzusetzen, wenn sich die Menge der Frauen
# im gebärfähigen Alter nicht ändern würde, und ihre Alterszusammensetzung
# konstant bliebe. Dem ist aber nicht exakt so.
# Wir setzen darum konservativ die Hälfte der Steigerungen an:
# West: +2,65%    Ost: +3,65%
# Da die Geburtenneigung in den Stadtstaaten niedriger ist, nehmen wir 
# für die drei Prognose-Regionen an:
# FW: +2,8%    FO: +3,8%    ST: +2,0%


deltas <- data.frame(region = c("FW", "FO", "ST"), 
				 delta = c(0.028, 0.038, 0.02),
				 stringsAsFactors = FALSE)

jahrgaenge_noz3 <- jahrgaenge_noz2 %>%
	left_join(deltas) %>%
	mutate(`0_bis_u1_noz` = ifelse(is.na(`0_bis_u1_noz`), (1+delta)*`0_bis_u1_KO`, `0_bis_u1_noz`)) %>%
	mutate(`1_bis_u2_noz` = ifelse(is.na(`1_bis_u2_noz`), (1+delta)*`1_bis_u2_KO`, `1_bis_u2_noz`)) %>%
	mutate(`2_bis_u3_noz` = ifelse(is.na(`2_bis_u3_noz`), (1+delta)*`2_bis_u3_KO`, `2_bis_u3_noz`)) %>%
	mutate(kinder_u3_noz = `0_bis_u1_noz` + `1_bis_u2_noz` + `2_bis_u3_noz`) %>%
	mutate(diff_u3_noz_KO = kinder_u3_noz - u3_KO)
	
# ---- IIId: Eigentliche Prognose ----
# Endziel: Aus ppf_noz die all_time_high-Summe für 2025 berechnen. Startjahr ist 
# zu Vergleichszwecken das Basisjahr 2016 (wie in PPF_2017).
# Model I: mit angepassten NOZ-Geburten und projizierten NOZ-Bedarfsquoten ab 2018:

# Tagespflege-Quoten festlegen (aus PPF Tabelle 12, gemäß KJH-Statistik 2016, also 2016er-IST-Verhältnisse festgehalten):
tp_quoten <- data.frame(region = c("FW", "FO", "ST"), 
				 tp_quote = c(17.1, 10.0, 9.2),
				 stringsAsFactors = FALSE)

model_I <- jahrgaenge_noz3 %>%
	select(jahr, region, kinder_u3_noz) %>%
	left_join(bedarfsquoten, by = c("jahr", "region")) %>%
	left_join(tp_quoten) %>%
	mutate(platzbedarf_u3 = round(kinder_u3_noz * bedarfsquote_u3/100)) %>%
	mutate(tp_bedarf_u3 = round(platzbedarf_u3 * tp_quote/100)) %>%
	mutate(kita_bedarf_u3 = round(platzbedarf_u3 * (100-tp_quote)/100))

referenz_2016 <- ppf %>%
	filter(typ=="Ist_PPF_2017") %>%
	left_join(tp_quoten) %>%
	mutate(tp_plaetze_u3 = round(plaetze_u3 * tp_quote/100)) %>%
	mutate(kita_plaetze_u3 = round(plaetze_u3 * (100-tp_quote)/100)) %>%
	select(region, referenz_u3_2016 = plaetze_u3, 
		  referenz_kita_u3_2016 = kita_plaetze_u3,
		  referenz_tp_u3_2016 = tp_plaetze_u3)

model_I_max_regionen <- model_I %>%
	group_by(region) %>%
	summarize(max_platzbedarf_u3 = max(platzbedarf_u3),
			max_platzbedarf_kita_u3 = max(kita_bedarf_u3),
			max_platzbedarf_tp_u3 = max(tp_bedarf_u3)) %>%
	left_join(referenz_2016) %>%
	mutate(max_ausbaubedarf_u3 = max_platzbedarf_u3 - referenz_u3_2016,
		  max_ausbaubedarf_kita_u3 = max_platzbedarf_kita_u3 - referenz_kita_u3_2016,
		  max_ausbaubedarf_tp_u3 = max_platzbedarf_tp_u3 - referenz_tp_u3_2016)

model_I_max_de <- model_I_max_regionen %>% 
	mutate_if(is.numeric, sum) %>%
	filter(region == "ST") %>% 
	mutate(region = "DE")

# Aus NOZ-Prognose für 3 Regionen ebenfalls für ganz DE
# die Bedarfsquoten rückrechnen

model_I_RevEng_de <- model_I %>%
	select(jahr, region, platzbedarf_u3_noz = platzbedarf_u3, kinder_u3_noz) %>%
	group_by(jahr) %>%
	summarize(platzbedarf_u3_noz = sum(platzbedarf_u3_noz), kinder_u3_noz = sum(kinder_u3_noz)) %>%   # Absolute Zahlen über 3 Regionen summieren
	mutate(bedarfsquote_noz_RevEng = round(platzbedarf_u3_noz/kinder_u3_noz*100,2)) %>%       # Rückgerechnete PPF-Quote
	full_join(select(bedarf_bund, jahr, quote,                  # Echte Zahlen inkl. Bedarfsquoten 2006 - 2017/18 dazu
				  bedarfsquote_u3, plaetze, kinder_u3)) %>%
	select(jahr, kinder_u3, kinder_u3_noz, plaetze, platzbedarf_u3_noz, quote, 
		  bedarfsquote_u3, bedarfsquote_noz_RevEng) %>%
	full_join(select(ppf_RevEng_de, jahr, platzbedarf_u3_ppf =plaetze_u3_ppf,          # Zahlen aus PPF-Prognose inkl. RevEng Bedarfsquoten dazu
				  bedarfsquote_ppf_RevEng, kinder_u3_ppf = kinder_u3)) %>%
	select(jahr, kinder_u3, kinder_u3_noz, kinder_u3_ppf, plaetze, platzbedarf_u3_noz, platzbedarf_u3_ppf,
		  quote, bedarfsquote_u3, bedarfsquote_noz_RevEng, bedarfsquote_ppf_RevEng) %>%
	mutate(region = "DE") %>%
	arrange(jahr)



# ---- IIIe: Investitionskosten f. Kitas berechnen: ----

kosten_kita_neubau_pp <- 36000     # Kosten pro Platz Vgl. PPF Tabelle 27, S. 41
kosten_kita_anbau_pp <- 18000

investitionen_kita_noz <- 0.5 * model_I_max_de$max_ausbaubedarf_kita_u3 * kosten_kita_neubau_pp +
	0.5 * model_I_max_de$max_ausbaubedarf_kita_u3 * kosten_kita_anbau_pp    # Je Hälfte der Plätze durch Neu- bzw- Anbau

investitionen_kita_ppf <- 4624165407 + 2312082704   # Invest.kosten laut PPF_2017; Summe aus Tabelle 29 S. 42

investitionen_kita_diff <- investitionen_kita_noz - investitionen_kita_ppf


# ---- IIIf: Personalmehraufwand berechnen ----

faktoren_mehrpersonal <- data.frame(region = c("FW", "FO", "ST"), 
					GT_faktor = c(0.9, 1.06, 0.99),            # Umrechnungsfaktoren für die Ganztagsbetreuungsäquivalente, siehe PPF_2017 Tabelle 15 S. 23
					pers_schluessel_kita = c(3.4, 5.7, 5.1),   # Personalschlüssel, siehe PPF_2017 Tabelle 16 S. 23
					pers_schluessel_tp = c(3.4, 4.1, 3.9),     # ... und fpr Tagespflege, ebd.
					VZ_faktor = c(1.26, 1.17, 1.2),            # Umrechnungsfaktoren der Vollzeitäquivalente, siehe PPF_2017 Tabelle 17, S. 23
				     stringsAsFactors = FALSE) %>%
	mutate(personalfaktor_kita = GT_faktor / pers_schluessel_kita * VZ_faktor,  # Gesamtfaktoren ausrechnen
		  personalfaktor_tp = GT_faktor / pers_schluessel_tp * VZ_faktor) %>%
	select(region, personalfaktor_kita, personalfaktor_tp)


mehrpersonal_regionen <- model_I_max_regionen %>%
	select(region, max_ausbaubedarf_kita_u3, max_ausbaubedarf_tp_u3) %>%
	left_join(faktoren_mehrpersonal) %>%
	mutate(mehrpersonal_kita = round(max_ausbaubedarf_kita_u3 * personalfaktor_kita),
		  mehrpersonal_tp = round(max_ausbaubedarf_tp_u3 * personalfaktor_tp))

mehrpersonal_de <- mehrpersonal_regionen %>%
	mutate_if(is.numeric, sum) %>%
	filter(region == "ST") %>%
	mutate(region = "DE") %>%
	select(starts_with("mehrper")) %>%
	mutate(mehrpersonal_gesamt = mehrpersonal_kita + mehrpersonal_tp)

mehrpersonal_noz <- mehrpersonal_de$mehrpersonal_gesamt
mehrpersonal_ppf <- 85086 + 14958 						# Personalmehrbedarf U3 Kita + Tagespflege laut PPF_2017; Summe aus Tabelle 19 S. 27
mehrpersonal_diff <- mehrpersonal_noz - mehrpersonal_ppf    # zusätzliches Mehrpersonal NOZ


# ---- IIIg: Berechn. für ganz DE bis 2025: Bedarfsquoten & Plätze (NOZ & Weiter-so) ----

# Ist-Werte
plaetze_vergleich_IST <- bedarf_bund %>%
	select(jahr, kinder_u3, quote, bedarfsquote = bedarfsquote_u3, plaetze) %>% 
	mutate(bedarfsquote = ifelse(jahr<2012, NA, bedarfsquote))

# Projektionen 2019-2025:
plaetze_vergleich_PROJ <- data.frame(jahr = 2019:2025, quote = NA)  # Data.Frame für 2019 bis 2025 anlegen

# U3-Kinder NOZ eintragen:
kinder_noz <- jahrgaenge_noz3 %>%
	filter(jahr >= 2019) %>%
	group_by(jahr) %>%
	summarize(kinder_u3 = sum(kinder_u3_noz)) %>%
	select(jahr, kinder_u3)

plaetze_vergleich_PROJ <- plaetze_vergleich_PROJ %>%
	left_join(kinder_noz)

# Projektion Plätze durch "weiter so", also wie im Durchschnitt seit 2015 (nach Hype wg. Recht auf Krippenplatz)
plaetze_2018 <- plaetze_vergleich_IST %>% filter(jahr==2018) %>% pull(plaetze)
plaetze_2015 <- plaetze_vergleich_IST %>% filter(jahr==2015) %>% pull(plaetze)
plaetze_2014 <- plaetze_vergleich_IST %>% filter(jahr==2014) %>% pull(plaetze)
plaetze_2012 <- plaetze_vergleich_IST %>% filter(jahr==2012) %>% pull(plaetze)
plaetze_2009 <- plaetze_vergleich_IST %>% filter(jahr==2009) %>% pull(plaetze)
plaetze_2006 <- plaetze_vergleich_IST %>% filter(jahr==2006) %>% pull(plaetze)

platzanstieg_2006_2014 <- round((plaetze_2014 - plaetze_2006)/(2014-2006))  # Durchschn. Antsieg pro jahr von 2006 bis 2014: 49.409
platzanstieg_2009_2014 <- round((plaetze_2014 - plaetze_2009)/(2014-2009))  # Durchschn. Antsieg pro jahr von 2008 bis 2014: 49.409
platzanstieg_2014_2018 <- round((plaetze_2018 - plaetze_2014)/(2018-2014))  # Durchschn. Antsieg pro jahr von 2014 bis 2018: 32.202

# Weiter-so-Mittelwert für Anstieg der Plätze:
# Wir nehmen das Mittel der Jahre *nach* den großen 
# Endspurt wg. Rechtsanspruch (1. Aug. 2013) bis heut.
# Also 2014-2018, bzw. Stand 1. März 2018 minus Stand 1. März 2014

steigung_plaetze <- platzanstieg_2014_2018  

plaetze_vergleich_PROJ <- plaetze_vergleich_PROJ %>%
	mutate( plaetze = round(plaetze_2018 + steigung_plaetze * (jahr-2018) ))

# Projektion Bedarfsquoten durch lineare Regression 2012-2017
x <- as.vector(2012:2017)
plaetze_vergleich_IST <- plaetze_vergleich_IST %>% arrange(jahr)
y <- filter(plaetze_vergleich_IST, jahr %in% x) %>% pull(bedarfsquote)
model_de <-lm(y ~ x)
summary(model_de)
x.new <- data.frame(x = 2018:2025)
y.new <- as.vector(round(predict(model_de, x.new),2))

plaetze_vergleich_IST$bedarfsquote[plaetze_vergleich_IST$jahr==2018] <- as.numeric(y.new[1]) # 2018er-Wert in IST-Tabelle eintragen
plaetze_vergleich_PROJ$bedarfsquote <-y.new[-1]  # Bedarfe laut linearem Model für 2019 - 2025 eintragen

# Data.Frames verbinden & fehlende Variablen berechnen
plaetze_vergleich <- bind_rows(plaetze_vergleich_IST, plaetze_vergleich_PROJ) %>%
	mutate( platzbedarf = round(kinder_u3 * bedarfsquote/100)) %>%   # Platzbedarf berechnen
	mutate( platzluecke = platzbedarf - plaetze)  %>%                # Platzlücke berechnen
	mutate( quote = ifelse(
		jahr >= 2019, round((plaetze / kinder_u3)*100, 2), quote)) %>% # Weiter-so-Quote 2019-2025
	mutate (quotenluecke = bedarfsquote - quote) %>%		# Quotenlücke 
	mutate( anteil_unerfuellt = round(quotenluecke / bedarfsquote *100, 1)) %>%   # Anteil Eltern mit unerfülltem U3-Wunsch
	mutate( delta_quote = quote - lag(quote)) %>%          	# Änderung der Quote (real oder weiter-so) zum Vorjahr
	mutate( delta_kinder_u3 = round(kinder_u3 - lag(kinder_u3))) %>%   # Änderung der U3-Kinder zum Vorjahr
	mutate( delta_plaetze = plaetze - lag(plaetze))	  		# Änderung der Plätze (real oder weiter-so) zum Vorjahr
	#	filter(jahr >= 2012)

# IV: Dataviz ----


# --- IVa: Bar Plot Plätze und Platzbedarf bis 2025 ----

matrix <- plaetze_vergleich %>% 
	filter(jahr >= 2012) %>%
	select(platzbedarf, plaetze) %>% t

jahre <- filter(plaetze_vergleich, jahr >=2012)$jahr
barplot(matrix, names.arg = jahre,
	   main = 'U3-Plätze und Platzbedarf', las = 1)

# --- IVb: Bar Plot Platz-Lücke bis 2025 ----

ppp <- filter(plaetze_vergleich, jahr >= 2012)
col <- c(rep("red", length(2012:2017)), rep("pink", length(2018:2025)))
jahreszahlen <- c("2012", paste("'", str_sub(ppp$jahr[-1], 3,4), sep=""))
barplot(-ppp$platzluecke, names.arg = jahreszahlen,
	    ylim = c(-370000, 0), las = 1, border=NA, 
	    col= col, axes = F)
for (h in seq(-50000, -300000, by=-50000)) { abline(h = h, col="white", lwd=2)}
axis(side = 2, at = seq(0,-350000, -50000), labels = seq(0, 350, 50), las = 1,
	lwd = 0, lwd.ticks = 1)    # Axen-Linie verstecken, aber Ticks zeichnen
mtext("U3-Betreuungslücke", side = 3, line = 2, cex=1.25, adj=0)
mtext("In 1.000 Plätzen", side = 3, line = 1, cex=1, adj = 0)
text(x = 3, y = -340000, "bisher", cex=.8, adj = c(0,1))
text(x = 8.5, y = -340000, "Prognose: Neue Plätze pro Jahr wie\n                  im Mittel 2014-2018", cex=.8, adj = c(0,1))

# # Bar Plot Änderung der Quote in %
# barplot(plaetze_vergleich$delta_quote, names.arg = plaetze_vergleich$jahr,
# 	   main = 'Änderung der U3-Quote in %', las = 1)
# 
# # Bar Plot Änderung der Plätze
# barplot(bedarf_bund$aenderung_plaetze, names.arg = bedarf_bund$jahr,
# 	   main = 'Änderung der Plätze', las = 1)

# --- IVcc: Bar Plots Zuwachs U3-Plätze & U3-Kinder bis 2025 ----

# Zuwachs U3-Plätze im Vergleich zum Vorjahr
# Jeweils zum 1.3. im Vergleich zum Vorjahr
ppp <- filter(plaetze_vergleich, jahr >= 2007)
col <- c(rep("darkgreen", length(2007:2018)), rep("lightgreen", length(2019:2025)))
jahreszahlen <- c("2007", paste("'", str_sub(ppp$jahr[-1], 3,4), sep=""))

barplot(ppp$delta_plaetze, names.arg = jahreszahlen,
	   ylim = c(0, 65000),
	   las = 1, border=NA, col= col,
	   axes = F)
for (h in seq(0, 60000, by=10000)) { abline(h = h, col="white", lwd=2)}
axis(side = 2, at = seq(0, 60000, by=10000), labels = c(seq(0, 50, by=10), "60\nTausend"), las = 1,
	lwd = 0, lwd.ticks = 1, cex.axis=0.8)    # Axen-Linie verstecken, aber Ticks zeichnen
mtext('Zuwachs U3-Plätze (real & "weiter so")', side = 3, line = 2, cex=1.25, adj=0)
mtext("Am 1. März im Vergleich zum Vorjahr", side = 3, line = 0.5, cex=1, adj = 0)
text(x = 4, y = 60000, "bisher", cex=0.8, adj = c(0,1))
text(x = 15, y = 50000, "Prognose:\nNeue Plätze pro Jahr wie\nim Mittel 2014-2018", cex=0.8, adj = c(0,1))

# --- IVd: Bar Plot Veränderung U3-Kinder bis 2025 ----

# Neue U3-Kinder im Vergleich zum Vorjahr
# ACHTUNG: Exakte Jahreszuweisung am Beispiel Maximum 2017: 103.628
# Zum 31.12.2016 (also quasi am 1.1.2017) gab es 103.628 mehr U3-Kinder als noch ein Jahr zuvor (also am 31.12.2015)

ppp <- filter(plaetze_vergleich, jahr >= 2007)
col <- c(rep("blue", length(2007:2018)), rep("lightblue", length(2019:2025)))
jahreszahlen <- c("2007", paste("'", str_sub(ppp$jahr[-1], 3,4), sep=""))

bp <- barplot(ppp$delta_kinder_u3, names.arg = jahreszahlen,
	   ylim = c(-35000, 120000), las = 1, border=NA, 
	   col= col, axes = F)
for (h in seq(-25000, 1000000, by=25000)) { abline(h = h, col="white", lwd=2)}
axis(side = 2, at = seq(-25000, 100000, by=25000), labels = c(seq(-25, 75, by=25), "100\nTausend"), las = 1,
	lwd = 0, lwd.ticks = 1, cex.axis=0.8)    # Axen-Linie verstecken, aber Ticks zeichnen
mtext('Veränderung U3-Kinder (real & Prognose)', side = 3, line = 2, cex=1.25, adj=0)
mtext("Anfang des Jahres im Vergleich zum Vorjahr", side = 3, line = 0.5, cex=1, adj = 0)
text(x = 3, y = 60000, "bisher", cex=1, adj = c(0,1))
text(x = 17, y = 60000, "Prognose", cex=1, adj = c(0,1))


# --- IVe: Dot Plot Quoten (real & Bedarf) bis 2025 ----

# Leeren Plot vorbereiten:
ppp <- filter(plaetze_vergleich, jahr >= 2006)
plot(x=ppp$jahr, y=ppp$quote, type = 'n', pch = 19, 
	xlim = c(2005,2025), ylim = c(-2,63), 
	xlab ='', ylab='', 
	las=1, bty = 'n', axes=F)

col_real <- "#666666"
col_bedarf <- "green"
col_area <- "#cccccc"
col_area_text <- "#000000"

# X-Achse:
axis(side = 1, at = c(2006, seq(2010,2025, by=5)), las = 1,
	lwd = 0, lwd.ticks = 1, cex.axis=1)    # Axen-Linie verstecken, aber Ticks zeichnen
# Y-Achse:
axis(side = 2, at = seq(0,60, by=10), labels = paste(seq(0,60, by=10), "%", sep = ""), las = 1,
	lwd = 0, lwd.ticks = 1, cex.axis=1)    # Axen-Linie verstecken, aber Ticks zeichnen
# Prognose-Zeitraum schattieren
rect(2017.5, 0, 2025.5, 60, col = "#dddddd", border = NA)

ppp <- ppp %>% arrange(jahr)
points(x=ppp$jahr, y=ppp$quote, pch = 19, col =col_real)			# Reale quoten
lines(x=ppp$jahr, y=ppp$quote, lwd=2, col =col_real)				# Quoten mit Linie verbinden
points(x=ppp$jahr, y=ppp$bedarfsquote, pch = 19, col =col_bedarf)	# Bedarfsquoten

# Trendlinie aus Deutschland-Modell dazu:
x.new <- data.frame(x = c(2017, 2025))
y.new <- as.vector(round(predict(model_de, x.new),2))
lines(x.new$x, y.new, col = col_bedarf, lty = 1, lwd = 1.5)

# Beschriftung:
mtext('Wird der Bedarf gedeckt?', side = 3, line = 2, cex=1.25, adj=0)
mtext("Betreuungsquoten für Kinder unter 3 Jahren", side = 3, line = 0.5, cex=1, adj = 0)
text(x = 2010, y = 45, "Bedarf", cex=.8, adj = c(0,1), col = col_bedarf)
text(x = 2007, y = 28, "Verfügbare\nBetreuung", cex=.8, adj = c(0,1), col = col_real)
text(x = 2018, y = 18, "Prognose:\n\nBedarf folgt Trend von\n2012-2017,\nverfügbare Plätze wachsen\nwie 2014-2017", 
	cex=.8, adj = c(0,1), col = col_area_text)

# --- IVf: Dot Plot Bedarfsquoten Fläche_West NOZ vs. bis 2025 ----

# Alle nötigen Quoten für 3 Regionen zusammensammeln:
# Historische Quoten und Bedarfe zuerst:
quoten_regionen_IST <- fortschreibung_bedarfe_u3_regionen %>%
	select(jahr, region, quote_u3, bedarfsquote_u3) %>%
	mutate(bedarfsquote_ppf = NA)

# Projizierte Quoten und Bedarfe:
quoten_regionen_PROJ <- model_I %>%
	select(jahr, region, bedarfsquote_u3) %>%
	left_join(select(ppf_check_prognose, jahr, region, 
				  bedarfsquote_ppf = bedarfsquote_ppf_RevEng))

# Beide verbinden:
quoten_regionen <- bind_rows(filter(quoten_regionen_IST, jahr <=2015),
					    quoten_regionen_PROJ) %>%
	select(-quote_u3) %>%
	left_join(select(quoten_regionen_IST, jahr, region, quote_u3))

# Daten speichern:
write.table(quoten_regionen, "data/quoten_regionen.tsv", 
		  sep = "\t",            # Tab als Trennzeichen 
		  dec = ",",             # Komma als Dezimaltrenner
		  na = "NA",             # NA-Wert für Excel auch na ="" oder na ="#NV"
		  row.names = FALSE, 
		  fileEncoding = "UTF-8")

saveRDS(model_Flaeche_West, file="data/model_Flaeche_West.rds") 
saveRDS(model_Flaeche_Ost, file="data/model_Flaeche_Ost.rds") 
saveRDS(model_Stadtstaaten, file="data/model_Stadtstaaten.rds") 


# Nur für Fläche_West:
quoten_FW <- filter(quoten_regionen, region == "FW")

# PLOT:

# Leeren Plot vorbereiten:
ppp <- quoten_FW
plot(x=ppp$jahr, y=ppp$quote_u3, type = 'n', pch = 19,
	xlim = c(2011,2025), ylim = c(15,65),
	xlab ='', ylab='',
	las=1, bty = 'n', axes=F)

col_real <- "#666666"
col_bedarf_messung <- "darkgreen"
col_noz <- "#00aa00"
col_ppf <- "blue"
col_unc <- "#dddddd"
cex_quoten <- 1.5

# X-Achse:
axis(side = 1, at = 2012:2025,
	labels = c(2012, NA, NA,  
			 2015, NA, NA, NA, NA,
			 2020, NA, NA, NA, NA,
			 2025), las = 1,
	lwd = 0, lwd.ticks = 1, cex.axis=1)    # Achsen-Linie verstecken, aber Ticks zeichnen
# Y-Achse:
axis(side = 2, at = seq(20,60, by=10), labels = paste(seq(20,60, by=10), "%", sep = ""), las = 1,
	lwd = 0, lwd.ticks = 1, cex.axis=1)    # Achsen-Linie verstecken, aber Ticks zeichnen

# Unsicherheitsbereich und Trendlinie aus FW-Modell zuunterst zeichnen:
x.new_unc <- data.frame(x = 2018:2025)
y.new_unc <- predict(model_Flaeche_West,x.new_unc , interval = "prediction")
polygon(y = c(y.new_unc[,2], rev(y.new_unc[,3])), 
	   x = c(x.new_unc$x, rev(x.new_unc$x)), 
	   col = col_unc, border = 0)

x.new <- data.frame(x = c(2012, 2025))
y.new <- as.vector(round(predict(model_Flaeche_West, x.new),2))
lines(x.new$x, y.new, col = col_noz, lty = 3, lwd = 1)


# # Querlinie für Level des finalen PPF-Bedarfs:
# ppf_bedarf_final <- ppp %>% filter(jahr==2025) %>% pull(bedarfsquote_ppf)
# lines(x = c(2012, 2025), y = c(ppf_bedarf_final, ppf_bedarf_final), 
# 	 col = col_ppf, lty = 2, lwd = 1)

# Dann Quoten dazu:
ppp_real <- filter(ppp, jahr <=2017)
points(x=ppp_real$jahr, y=ppp_real$quote_u3, pch = 19, cex = cex_quoten, col =col_real)			# Reale quoten
ppp_messung <- filter(ppp, jahr <=2017)
points(x=ppp_messung$jahr, y=ppp_messung$bedarfsquote_u3, pch = 19, cex = cex_quoten,  
	  col =col_bedarf_messung)	# Bedarfsquoten gemessen
ppp_noz <- filter(ppp, jahr >=2018)
points(x=ppp_noz$jahr, y=ppp_noz$bedarfsquote_u3, pch = 1, cex = cex_quoten, 
	  col =col_noz)	# Bedarfsquoten NOZ
ppp_ppf <- filter(ppp, jahr >=2017)
points(x=ppp_ppf$jahr, y=ppp_ppf$bedarfsquote_ppf, pch = 1, cex = cex_quoten, 
	  col =col_ppf)	# Bedarfsquoten PPF



# Beschriftung:
mtext('Wie wird sich der Bedarf an U3-Betreuung entwickeln?', side = 3, line = 2, cex=1.25, adj=0)
mtext("Annahmen der NOZ und der Regierungsprognose für westdeutsche Flächenländer\n(Anteil Kinder in Betreuung)", side = 3, line = -0.5, cex=1, adj = 0)
text(x = 2013, y = 45, "Erhobener Elternwunsch", cex=1, adj = c(0,1), col = col_bedarf_messung)
bedarf_2017 <- paste("2017:\n", round(filter(ppp, jahr == 2017)$bedarfsquote_u3,1), "%", sep="")
text(x = 2017, y = 40, bedarf_2017, cex=1, adj = c(0.5,1), col = col_bedarf_messung)
quote_2017 <- paste("2017:\n", round(filter(ppp, jahr == 2017)$quote_u3,1), "%", sep="")
text(x = 2017, y = 25, quote_2017, cex=1, adj = c(0.5,1), col = col_real)

text(x = 2013, y = 22, "Tatsächlich verfügbare\nBetreuung", cex=1, adj = c(0,1), col = col_real)
text(x = 2019, y = 55, "NOZ-Prognose:\nBedarf folgt Trend", cex=1, adj = c(0,1), col = col_noz)
text(x = 2019, y = 28, "Regierungs-Prognose:\n'Elternwünsche' erreichen 2024\nden 2017 gemessenen Wert", cex=1, adj = c(0,1), col = col_ppf)
bedarf_ppf_2025 <- paste("2025:\n", round(filter(ppp, jahr == 2025)$bedarfsquote_ppf,1), "%", sep="")
text(x = 2025, y = 40, bedarf_ppf_2025, cex=1, adj = c(1,1), col = col_ppf)
bedarf_noz_2025 <- paste("2025:\n", round(filter(ppp, jahr == 2025)$bedarfsquote_u3,1), "%", sep="")
text(x = 2025, y = 50, bedarf_noz_2025, cex=1, adj = c(1,1), col = col_noz)



