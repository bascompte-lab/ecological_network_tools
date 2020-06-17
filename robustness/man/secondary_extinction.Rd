\name{secondary_extinction}
\alias{secondary_extinction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Secondary extinction of consumers by progressive removal of resources
}
\description{
This function simulates the secondary extinction of a consumer population (e.g., pollinators) as resources (e.g., plants) are progressively removed.
}
\usage{
remove_plants(my_network, 20, "RND")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bipartite_network}{dataframe representing the incidence matrix relating two species, rows and columns
   }
  \item{n_iter}{integer, number of iterations for randomized removal strategies (see below)
}
  \item{strategy}{string that defines the removal startegy; possible values: \cr
  "RND" = randomly \cr
  "LTM" = from least connected (most specialized) to most connected (most generalist) \cr
  "MTL" = from most connected (most generalist) to least connected (most specialized)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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

## A sample dataset downloaded from the http://www.web-of-life.es/ database \cr
## is stored in the "data" folder; we load that data in a variable
NW_PL_062 <- load("data/weboflife_PL_062.RData")

## To simulate secondary extinction of pollinators removing plants at random:
remove_plants(NW_PL_062, 2, "RND")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~foodwebs }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mutualistic networks }% __ONLY ONE__ keyword per line
\keyword{ ~networks robustness}% __ONLY ONE__ keyword per line
