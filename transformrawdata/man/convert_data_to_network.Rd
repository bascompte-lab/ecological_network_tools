\name{convert_data_to_network}
\alias{convert_data_to_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform raw data into a bipartite graph. Data can also be filtered by specific values of any covariables.
}
\description{
This funciton transforms raw data into a bipartite graph. It requires igraph.
}
\usage{
convert_data_to_network(df, covariable_to_filter_by, level_of_covariable_to_filter_by)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{dataframe}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A bipartite igraph graph. In other words, an igraph graph that has a vertex attribute type.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## df is an incidence matrix as dataframe
bipartite_graph_from_raw_data(df)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bipartite graph}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
