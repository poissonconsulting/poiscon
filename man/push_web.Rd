\name{push_web}
\alias{push_web}
\title{Publishes web respository}
\usage{
push_web(dir = "poissonconsulting.github.io",
  message = paste0(project_folder(), " ", Sys.time()))
}
\arguments{
  \item{dir}{string of jekyll repository directory}

  \item{message}{string of commit message}
}
\value{
Git commits
}
\description{
Pushes web respository
}

