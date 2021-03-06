# poiscon v0.10.1

- GitHub dependencies on Remotes

# poiscon v0.10.0

- added `print_plot()` function to catch errors when printing plots
- `save_plot` now has argument `plot = ggplot2::last_plot()`

# poiscon v0.9.0

- updated `theme_Poisson()` based on ggplot2 v2.0.0
- `theme_set(theme_Poisson())` no longer automatically called at load

# poiscon v0.8.22

- added examples for `standardise_datetime` which now actually uses UTC instead of GMT

# poiscon v0.8.21

- `save_plot` saves as png and pdf

# poiscon v0.8.20

- `remove_dots_colnames_data_frames` also removes spaces

# poiscon v0.8.19

- `perform_analyses` function now works with list of vectors
- `perform_analyses` has mode argument to temporarily set `opts_jagr`
- `perform_analyses` has beep argument to switch off beeping

# poiscon v0.8.18

- added `perform_analyses` function
- added `plot_residuals_analyses` function
- added `remove_dots_colnames_data_frames` function
- removed inhouse functions (as masking reporting)

# poiscon v0.8.17

* `set_folders()` returns file address for printing

# poiscon v0.8.16

* added `load_value()` to get values from saved tables

# poiscon v0.8.15

- upgraded to jaggernaut 2.1.1 - no longer calculates dic
- generates x11 for linux
- switched from model_number to model for new jaggernaut

# poiscon v0.8.14

- updated ggplot2 Poisson theme to include panel.grid.minor and panel.grid.major

# poiscon v0.8.13

- updated ggplot2 themes to include panel.margin.x and panel.margin y

# poiscon v0.8.12

- changed `delete_directory` to not delete output/rdata/input by default

# poiscon v0.8.11

- moved `perform_analyses` into `functions.R` script of skeleton

# poiscon v0.8.10

- added `diel_period()`

# poiscon v0.8.9

- `delete_output()` now has argument to keep output/rdata/input folder
- tidied up namespace so only import packages that uses
- colors now c("black", "red", "blue", "green4", "orange3", "slategray")
- uses `tulip` v0.0.7 and `jaggernaut` v1.8.0

# poiscon v0.8.8

- save_rdata(object, name = NULL) now uses object name if
name is unspecified and if object is unspecified saves all
data.frames in global environment as individual objects
using own names
- load_rdata(name = NULL) now loads all objects in
current rdata folder is name is unspecified into 
global env

# poiscon v0.8.7

- load_table iuses `csv` if `rds` file not available
- specified package version numbers and moved `dplyr` to last
- added `delete_plots` and `delete_output` functions

### poiscon 0.7

- major reconfiguration with other packages

## poiscon v0.6.11

- added `sql_datetime`. `extract_datetime` and `cleanup_datetime` families of functions
to handle timezones when inputting from access database

- added `ping()` to signal completion of `perform_analyses` function

## poiscon v0.6.10

- `perform_analyses` does test of prediction and residual in derived code
when in mode debug

## poiscon v0.6.9

- `knit_report` now uses `render_jekyll()`

## poiscon v0.6.8

- added *deck- functions `author-deck`, `slidify-deck` and `deck-to-web` 
on top of slidify library for presentations

# poiscon 0.6.7

- added `copy_web` and `push_web` functions to control jekyll repositories
- added `knit-report` to convert report to .md., .docx and .html
- added `figures-report` and `figures-web` to extract .pngs

# poiscon 0.6.6

- added `install_hd` function to simplify installing packages from harddrive
- added internal `git_commit` function to commit (and push) for current (or other git repository)

# poiscon 0.6.5

- corrected spacing `knit_models`

# poiscon 0.6.4

- added upload-files function

# poiscon 0.6.3

- added extract-figures, project-folder, report-url and repository-url functions

# poiscon 0.6.2

- Fixed bug `knit_...` functions not resetting folders.

# poiscon 0.6.1

- Fixed bug `knit_models` not sourcing description and models.

# poiscon 0.6

- Added `perform_analyses` function to run a set of analyses in a project folder.

# poiscon 0.5

- Added `knitr_parameters`, `knit_figures` and `knit_models` functions to 
create markdown files of parameter estimates and figures and models.

- Fixed conflict over here function that was imported from plyr and lubridate
packages using @importFrom.

# Bug Reports 

For more fine-grained list of changes or to report a bug, consult 

- [The commit log](https://github.com/poissonconsulting/poiscon/commits/master)
- [The issues log](https://github.com/poissonconsulting/poiscon/issues)

# Versioning

Releases are numbered with the following semantic versioning format:

\<major\>.\<minor\>.\<patch\>

And constructed with the following guidelines:

- Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
- New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
- Bug fixes and misc changes bumps the patch
