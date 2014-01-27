# poiscon 0.6.11

* added `sql_datetime`. `extract_datetime` and `cleanup_datetime` families of functions
to handle timezones when inputting from access database

* added `ping()` to signal completion of `perform_analyses` function

# poiscon 0.6.10

* `perform_analyses` does test of prediction and residual in derived code
when in mode debug

# poiscon 0.6.9

* `knit_report` now uses `render_jekyll()`

# poiscon 0.6.8

* added *deck* functions `author-deck`, `slidify-deck` and `deck-to-web` 
on top of slidify library for presentations

# poiscon 0.6.7

* added `copy_web` and `push_web` functions to control jekyll repositories
* added `knit-report` to convert report to .md., .docx and .html
* added `figures-report` and `figures-web` to extract .pngs

# poiscon 0.6.6

* added `install_hd` function to simplify installing packages from harddrive
* added internal `git_commit` function to commit (and push) for current (or other git repository)

# poiscon 0.6.5

* corrected spacing `knit_models`

# poiscon 0.6.4

* added upload-files function

# poiscon 0.6.3

* added extract-figures, project-folder, report-url and repository-url functions

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
