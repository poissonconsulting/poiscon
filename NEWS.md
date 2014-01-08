# NEWS

# poiscon 0.6.2

* Fixed bug `knit_...` functions not resetting folders.

# poiscon 0.6.1

* Fixed bug `knit_models` not sourcing description and models.

# poiscon 0.6

* Added `perform_analyses` function to run a set of analyses in a project folder.

# poiscon 0.5

* Added `knitr_parameters`, `knit_figures` and `knit_models` functions to 
create markdown files of parameter estimates and figures and models.

* Fixed conflict over here function that was imported from plyr and lubridate
packages using @importFrom.

# Bug Reports 

For more fine-grained list of changes or to report a bug, consult 

* [The commit log](https://github.com/poissonconsulting/poiscon/commits/master)
* [The issues log](https://github.com/poissonconsulting/poiscon/issues)

# Versioning

Releases are numbered with the following semantic versioning format:

\<major\>.\<minor\>.\<patch\>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc changes bumps the patch
