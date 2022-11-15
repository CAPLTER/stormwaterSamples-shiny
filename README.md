# stormwaterSamples-shiny

## R shiny application to facilitate entry, upload, and quality control for CAP LTER stormwater sample, discharge, chemistry, and particulate data

#### overview

This is the codebase for the Shiny application that facilitates data entry and quality control for the CAP LTER's long-term monitoring of urban stormwater. This project has a long history focusing on different sites and lotic systems throughout the greater Phoenix metropolitan area, but focuses currently on a small number of locations along the Indian Bend Wash almost entirely within the city of Scottsdale, Arizona.

The application expands on several new approaches to Shiny development first introduced in the [herpetofauna application](https://gitlab.com/caplter/herpetofauna-shiny) then later adopted and expanded upon in the [bird application](https://gitlab.com/caplter/core-birds-shiny). A notable addition in this version of the Stormwater application is the use of a proxy coupled to an editable dataTables object to facilitate editing sample data uploaded from the ISCO 6700. In hindsight, use of a proxy might be overkill in this context as the primary purpose of using a proxy is to not requite that the dataTables object be (re)rendered with each edit, but the sample data from any given report is really a quite small table so processing overhead is not (at least, should not) be a concern. Anyway, it is done and now serves as a model for applying the approach in other contexts. Excellent resources that aided the implementation of the proxy included:

- [R Shiny and DataTable (DT) Proxy Demonstration For Reactive Data Tables](https://thatdatatho.com/r-shiny-data-table-proxy-replace-data/?utm_source=pocket_mylist)
- [DataTables from the DT package as a Shiny CRUD app interface](https://www.travishinkelman.com/dt-datatable-crud/)



#### previous versions history

UPDATE 2020-01-08 (version 2): This marks a substantial rewrite of the application. Code for samples, solids, and viewing discharge was moved to modules, UI/UX vastly improved, and the DB connection was moved to a pool. Uploading discharge data was the only component that was not updated. Note that while working with solids was not moved to module, the code was updated considerably. Solids was not moved to a module as it was not possible to pass a messenger to the main app (home of the main solids data view) indicating updates - of course, it is possible by returning a value from the module and then assigning the call to the module to an object, but this could only be done in the observeEvent call where the module was instantiated thus nullifying the ability of the messenger to update the reactive in the app.

UPDATE 2018-06-20: Added functionality to view discharge data in the database. Changed discharge insert/upload functionality to do nothing (i.e., skip insert/upload) for duplicate samples - this allows, for example, a Flowlink file with a mix of new and already uploaded data to be uploaded without having to manually remove the data that are already in the database. Added pagination to samples viewer.
