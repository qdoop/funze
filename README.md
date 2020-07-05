# funze

An attempt to build a concatenative languange (Forth like) on top of F#

Should be able to compile, invoke and run F# fragements and invoke operations through reflection on F# objects

Currently basic math operations fragment invocation and dot member access are implemented. 

Project needs heavy clean up and several work to add features as project file states fz0clean.fsproj 

Restore the project and run it. You can use nodemon (nodejs) for build automation(see tasks.json).

Open localhost:8080 for writing scripts and see execution output.
In scripts folder there are same sample scripts to load and execute

NOTE: Select the endpoint when opening the browser page to connect with the script engine.