\name{install_hd}
\alias{install_hd}
\title{Installs package from hard drive}
\usage{
install_hd(pkg = "poiscon", code_dir = options()$code_dir)
}
\arguments{
  \item{pkg}{string of repository directory name}

  \item{code_dir}{string of path to repository directory}
}
\value{
Installs package
}
\description{
Commits changes to github repository and pushes to remote
}

