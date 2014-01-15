\name{publish_deck}
\alias{publish_deck}
\title{Publish poiscon}
\usage{
publish_deck(deck_dir = "deck", repo_dir = "poissonconsulting.github.io",
  repo_path = "~/Documents/code")
}
\arguments{
  \item{deck_dir}{string of deck directory (in
  output/decks).}

  \item{repo_dir}{string of jekyll directory name.}

  \item{repo_path}{string of path to jekyll directory}
}
\value{
Transfers deck
}
\description{
Currently just publishes slidify to clone of jekyll repo.
}

