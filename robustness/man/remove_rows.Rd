\name{remove_rows}
\alias{remove_rows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate secondary extinction of consumers by progressive removal of resources
}
\description{
This function simulates the secondary extinction of a consumer population (e.g., pollinators) as resources (e.g., plants) are progressively removed.
}
\usage{
remove_rows(bipartite_network, n_iter, strategy, i_seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bipartite_network}{dataframe, incidence matrix relating two species, rows and columns
   }
  \item{n_iter}{integer, number of iterations for randomized removal strategies (see below)
}
  \item{strategy}{string that defines the removal startegy; possible values: \cr
  "RND" = randomly \cr
  "LTM" = from least connected (most specialized) to most connected (most generalist) \cr
  "MTL" = from most connected (most generalist) to least connected (most specialized)
}
   \item{i_seed}{integer, seed of the random sequence}
}
\details{
Required packages: fastmatch, permute
}
\value{
dataframe
%%  If it is a LIST, use
\item{removed_rows}{ number of resources removed}
\item{removed_rows}{ average number of consumers that go extinct after having removed a given number of resources}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alessandro Vindigni
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## download the package weboflife, which contains the function get_networks()
devtools::install_github("bascomptelab/ecological_network_tools/weboflife")
library(weboflife)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~foodwebs }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mutualistic networks }% __ONLY ONE__ keyword per line
\keyword{ ~networks robustness}% __ONLY ONE__ keyword per line
