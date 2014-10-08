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

* eightytwenty - find the 80/20 cutoff in a vector
* grepnames - search column names for a string
* histrug - plot a histogram with a rug
* muplot - sets formatting options for plots
* outliers - find the upper and lower bound for outliers
* read.hallp - read hallp.csv, optionally looking to see if it exists
* read.tidy - read a csv & tidy up column names, optionally looking to see if the object exists
* write.clip - write an object to the clipboard
* renormalize - turn a denormalized list of values back into id/value pairs
