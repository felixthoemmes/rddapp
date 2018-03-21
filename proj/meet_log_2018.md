## January, 31st

- Irena and Wenyu will glance at the narrative of the grant
- Irena will familiarize herself with the Shiny source code from Wang
- Wenyu will familiarize herself with the R source code from Ze
- Felix will continue to write the help file, and will ask Irena for help with screenshots, etc. 
- Tracey Sweet, editor of the software section of JEBS, emailed us to initiate a review of our package to be published in JEBS - the deadline for this is the first week of March

- we changed the regular meeting time to be 1:30pm in MVR G62A


## February, 14th

- Felix and Wenyu discussed the possiblity of writing a summary function for mrd_est objects (results of MRDD runs). Here is a pointer on how to write summary functions for custom classes: https://stackoverflow.com/questions/18684229/how-to-get-summary-to-work-with-custom-class-in-r

- Felix will write some help file materials for Irena to check, edit, and integrate into the Shiny app


## February, 21th

- Irena and Felix met and discussed how to write the help manual of the shiny app. Irena will try to write a script that will compilea all help pages, and feed them into an R program that uses Shiny-specific commands to generate an html for us.

- Felix will continue writing the help pages.

- Irena and Felix also discussed the use the github projects pages. We will use them going forward to assign issues. 

## February, 28th

- Irena briefed us on her attempts to integrate compiled Markdown into the shiny app. She is still looking for a solution. 
- Wenyu and Felix checked the newly written summary functions. They seemed to work well. We decided to also add some confidence interval functionality to the summary output, and Felix created an issue on github. 

## March, 7th

- Wenyu and Felix met and discussed the recent changes to the summary function. 
- Felix also pointed out some minor improvements in the dc_est function and the manual in the package; those were all converted to issues on the project page. 
- Felix added a download counter to our github page. Average downloads per month are at 240

## March, 21st
 - Wenyu fixed many of the issues from the projects page. One thing that became apparent was that the mfrd_est function has an argument for tr (treatment assignment), however this argument does nothing. It's intended use was to support fuzzy assigment, but we don't support that model (MRDD frontier approach) - yet. 
