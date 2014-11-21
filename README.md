muadc
=====

an R package with everyday functions for my job.

## Variables

Contains various variables:

* currentFY  - current fiscal year
* fyStartDate - the date the current fiscal year started
* fyEndDate - the date the current fiscal year ended
* tmuGoal - the dollar goal for Transform MU
* afGoal - the dollar goal for the Millikin Fund

## Functions

* coalesce - an SQL-style coalesce function to return the first non-NA value in an assortment of vectors
* eightytwenty - find the 80/20 cutoff in a vector
* grepnames - search column names for a string
* histrug - plot a histogram with a rug
* initcommitsdb - initalizes dplyr-style connection to my local MySQL giving datawarehouse
* is.blank - an R-style is.foo test to see if a field is blank, purposefully more generalized than `is.na`
* muplot - sets formatting options for plots
* outliers - find the upper and lower bound for outliers
* read.hallp - read hallp.csv, optionally looking to see if it exists
* read.tidy - read a csv & tidy up column names, optionally looking to see if the object exists
* renormalize - a function to renormalize (in the Bryce-Codd sense of normalization) a set of key/value pair where the value contains multiple values
* renormalize - turn a denormalized list of values back into id/value pairs
* startup - do various typical startup tasks. Largely useless
* write.clip - write an object to the clipboard

## TODO 
* givingfactor - a function to bin giving (or wealth capacity) into various factor levels
* startup - revise, make useful or remove
