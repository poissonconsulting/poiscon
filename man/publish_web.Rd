\name{publish_web}
\alias{publish_web}
\title{Publishes web respository}
\usage{
publish_web(dir = "poissonconsulting.github.io", path = options()$code_path,
  message = paste0(project_folder(), " ", Sys.time()))
}
\arguments{
  \item{dir}{string of jekyll repository directory}

  \item{path}{string to repository directory}

  \item{message}{string of commit message}

  \item{pash}{flag of whether to push to remote}
}
\value{
Git commits
}
\description{
Publishes web respository
}

