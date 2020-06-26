
\name{get_networks}
\alias{get_networks}
\title{Get networks from Web of Life}
\usage{
get_networks(interaction_type, data_type, names = "yes")
}
\arguments{
\item{interaction_type}{"all", "plant-ant", "pollination", "seed disperser", "plant-herbivore", "anemone-fish", "host-parasite", "food web"}

\item{data_type}{"binary", "weighted", or "all"}

\item{names}{would you like to include the names of species? default = "yes", otherwise input "no"}
}
\value{
list of networks
}
\description{
Get networks from Web of Life
}

\usage{
get_network(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
