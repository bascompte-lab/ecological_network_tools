\name{filter_raw_data}
\alias{filter_raw_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filter raw data
}
\description{
This fucntion filters raw data loaded from a csv file by a specific value of a selected column.
}
\usage{
filter_raw_data(df, column_name, column_value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{dataframe}
  \item{column_name}{strting: name of the column w.r.t. which we want to filter}
  \item{column_value}{strting: value by which we want to filter}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The same format of the input dataframe is returned with filtered data
  \item{df}{dataframe}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fernando Pedraza
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## import df from csv file
df = read.csv("raw_data.csv", sep = ";")

## filter by the value = "green" of the column "color"
df_out <- filter_raw_data(df, "color", "green")

## filter as above plus by the value = "large" of the column "size"
df_out <- filter_raw_data(df, "color", "green") \%>\% filter_raw_data(., "size", "large")

}

\keyword{ ~filter }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~csv }% __ONLY ONE__ keyword per line
