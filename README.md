# stormwaterSamples-shiny

Shiny data entry application for stormwater samples, discharge, and AFDM

UPDATE 2020-01-08 (version 2): This marks a substantial rewrite of the
application. Code for samples, solids, and viewing discharge was moved to
modules, UI/UX vastly improved, and the DB connection was moved to a pool.
Uploading discharge data was the only component that was not updated. Note that
while working with solids was not moved to module, the code was updated
considerably. Solids was not moved to a module as it was not possible to pass a
messenger to the main app (home of the main solids data view) indicating updates
- of course, it is possible by returning a value from the module and then
assigning the call to the module to an object, but this could only be done in
the observeEvent call where the module was instantiated thus nullifying the
ability of the messenger to update the reactive in the app.

UPDATE 2018-06-20: Added functionality to view discharge data in the database.
Changed discharge insert/upload funtionality to do nothing (i.e., skip
insert/upload) for duplicate samples - this allows, for example, a Flowlink file
with a mix of new and already uploaded data to be uploaded without having to
manually remove the data that are already in the database. Added pagination to
samples viewer.