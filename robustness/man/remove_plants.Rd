\name{remove_plants}
\alias{remove_plants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate secondary extinction of consumers by progressive removal of resources
}
\description{
This function simulates the secondary extinction of a consumer population (e.g., pollinators) as resources (e.g., plants) are progressively removed.
}
\usage{
remove_plants(bipartite_network, n_iter, strategy, i_seed)
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
\item{removed_plants}{ number of plants removed}
\item{removed_animals}{ average number of animals that go extinct  after having removed a given number of plants}
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

## use the function get_networks() to download one network ("M_PL_062") from the the http://www.web-of-life.es/ database
my_network <- get_networks(network_name = "M_PL_062")

## transform the data into a suitable format for the funciton remove_plants()
NW_PL_062 <-  column_to_rownames(my_network, var="V1")

## simulate secondary extinction of pollinators removing plants at random:
remove_plants(NW_PL_062, 3, "RND", 125)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~foodwebs }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mutualistic networks }% __ONLY ONE__ keyword per line
\keyword{ ~networks robustness}% __ONLY ONE__ keyword per line
