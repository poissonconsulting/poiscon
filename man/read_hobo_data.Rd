\name{read_hobo_data}
\alias{read_hobo_data}
\title{Read hobo data}
\usage{
read_hobo_data(file, recursive = TRUE, quiet = TRUE)
}
\arguments{
  \item{file}{the location of the hobo file or folder with
  hobo files}

  \item{recursive}{a logical scalar indicating whether to
  read data from subdirectories}

  \item{quiet}{a logical scalar indicating whether to
  provide messages}
}
\value{
the data in the form of a data.frame
}
\description{
Reads hobo data into R
}

