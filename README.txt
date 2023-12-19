Installationen
> Installation MongoDBCompass
> Installation R und RStudio

Datenbank restore
>In Commandzeile zur Location des DB Dumps navigieren
	Beispiel: cd C:\Soundlogger_SNP\Dump\ClassifiedAudioSNP
>In Command MongoDB Dump restore ausführen
	mongorestore --uri "mongodb://localhost:27017" 

App starten
> RStudio starten 
> .Rproj-Datei in einem neuen Projekt öffnen
> Main Application befindet sich in app.R mit spezifischen functions in functions/functions.R
> app.R run

Übersicht Verzeichnisse und Dateien
> docs 
	enthält GitHubPages
> functions
	functions.R
		ausgelagerte Funktionen
> glyphs
	Speicherpfad Glyphen aus der aggregierten Analyse
> overview_sites
	Speicherpfad Glyphen aus der Overview
> renv
	Management für R-Projektumgebung - zum Upload auf shinyapps.io
> rsconnect
	Dient zur Bereitstellung der Inhalte aus RStudio in shinyapps.io
> www
	Speicherpfad der Bilder
> projekt_soundlogger.Rproj
	Speicherpfad R-Projekt
> app.R
	Main Application