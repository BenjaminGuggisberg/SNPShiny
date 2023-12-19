Installations
> Installation MongoDBCompass
> Installation R and RStudio

Restore database
>Navigate to the location of the DB dump in the command line
	Example: cd C:\Soundlogger_SNP\Dump\ClassifiedAudioSNP
>Execute restore in command MongoDB dump
	mongorestore --uri "mongodb://localhost:27017" 

Start the app
> Start RStudio 
> Open .Rproj file in a new project
> Main Application is located in app.R with specific functions in functions/functions.R
> app.R run

Overview of directories and files
> docs 
	contains GitHubPages
> functions
	functions.R
		outsourced functions
> glyphs
	Storage path glyphs from the aggregated analysis
> overview_sites
	Storage path glyphs from the overview
> renv
	Management for R project environment - for upload to shinyapps.io
> rsconnect
	Used to provide the content from RStudio in shinyapps.io
> www
	Storage path of the images
> project_soundlogger.Rproj
	Storage path R project
> app.R
	Main Application
