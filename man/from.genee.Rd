\name{from.genee}
\alias{from.genee}
\title{from.genee}
\description{
  R interface to get selection from GENE-E.
}
\usage{
from.genee(url='http://localhost:9998')
}
\arguments{
  \item{url}{GENE-E URL.}
}
\value{
  List containing row.annotations, column.annotations, and matrix.
}
\details{
  Gets selection from GENE-E. If no rows or columns are selected, the full dataset is returned.
}


\examples{
# not run
# z = from.genee();
}


