\name{upload_bch_data}
\alias{upload_bch_data}
\title{Upload BCH data}
\usage{
  upload_bch_data(data,
    access_file = "../../../bchydro_data_12/data/based/upload/ColDuncEnv.accdb")
}
\arguments{
  \item{data}{the data.frame to upload}

  \item{access_file}{an character scalar indicating the
  path to the database.}
}
\value{
  Uploads data to the uplopad table of the database.
}
\description{
  Uploads BCH data to database.
}

