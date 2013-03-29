\name{to.genee}
\alias{to.genee}
\title{to.genee}
\description{
  R interface to view a matrix in GENE-E
}
\usage{
to.genee(mdat, row.annotations=NULL, column.annotations=NULL, show.rownames=T, show.colnames=T, row.hclust=NULL, column.hclust=NULL, url='http://localhost:9998')
}
\arguments{
  \item{mdat}{a matrix.}
  \item{row.annotations}{a data frame containing row annotations.}
  \item{column.annotations}{a data frame containing column annotations.}
  \item{show.rownames}{Whether to show the row names of the matrix in GENE-E.}
  \item{show.colnames}{Whether to show the column names of the matrix in GENE-E.}
  \item{row.hclust}{An object of class hclust to show in GENE-E. If provided, the matrix and annotations will be reordered to reflect the tree produced by the clustering process.}
  \item{column.hclust}{An object of class hclust to show in GENE-E. If provided, the matrix and annotations will be reordered to reflect the tree produced by the clustering process.}
  \item{url}{GENE-E URL.}
}
\details{
  R interface to GENE-E.
}

\examples{
# not run
# z = matrix(rnorm(30),nrow=5,ncol=6);
# row.names(z) <- LETTERS[1:NROW(z)];
# colnames(z) <- LETTERS[1:NCOL(z)];
# row.annotations <- data.frame(y=1:5, char = I(letters[1:5]))
# L3 <- LETTERS[1:3]  
# column.annotations <- data.frame(y=1:6, char = I(letters[1:6]), fac=sample(L3, 6, replace=TRUE))
# to.genee(z, row.annotations, column.annotations, column.hclust=hclust(dist(t(z))));
}


