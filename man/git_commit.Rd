\name{git_commit}
\alias{git_commit}
\title{Git commit (and push)}
\usage{
git_commit(dir = getwd(), message = paste0("poiscon::git_commit: ",
  Sys.time()), push = TRUE)
}
\arguments{
  \item{dir}{string of repository directory}

  \item{message}{string of commit message}

  \item{pash}{flag of whether to push to remote}
}
\value{
Git commits
}
\description{
Commits (and pushes) changes
}

