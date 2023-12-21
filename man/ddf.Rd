\name{ddf}
\alias{ddf}
\alias{ddf-package}
\title{
Compare Data Frames
}
\description{

  Compare the rows of data frames.
  
}
\usage{
ddf(new, old = NULL,
    by = NULL,
    ignore.case = FALSE,
    ignore.ws = FALSE,
    ignore.headers = FALSE,
    ignore = NULL,
    ignore.rows = NULL,
    ignore.columns = NULL,
    only.columns = NULL,
    ...)
}
\arguments{
  \item{new}{
    a \code{\link{data.frame}}
  }
  \item{old}{
    a \code{\link{data.frame}}
  }
  \item{by}{

    integer or character: the columns that are used to compare
    the data-frames. If 0, \code{\link{row.names}} are used.

  }
  \item{ignore.case}{
    logical
  }
  \item{ignore.ws}{
    logical
  }
  \item{ignore.headers}{
    logical
  }
  \item{ignore}{
    logical
  }
  \item{ignore.rows}{
    logical
  }
  \item{\dots}{
    other arguments
  }
}
\details{
  Very experimental.
}
\value{
  a list of three components
    %% \item{added}{new rows}
    %% \item{removed}{rows in old but not in new}
    %% \item{changed}{rows in both data-frames, but with changed contents}

  
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
%% \author{
%% %%  ~~who you are~~
%% }
%% \note{
%% %%  ~~further notes~~
%% }
%% %% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
 packages \pkg{compareDF} and \pkg{diffdf}
}
\examples{

old <- data.frame(key = 1:3, labels = c("a", "b", "c"))
new <- data.frame(key = 3:4, labels = c("d", "e"))
ddf(new, old, by = "key")

}
