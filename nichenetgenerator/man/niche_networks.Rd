\name{niche_networks}
\alias{niche_networks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Build a bipartite network following the niche model
%%  ~~function to do ... ~~
}
\description{
Builds a bipartite network of specified size and connectance using the niche model for food webs.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
niche_networks(num_sp_in_A,num_sp_in_B,empirical_connectance)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{num_sp_in_A}{
    The number of speices in group A
  }

  \item{num_sp_in_B}{
    The number of speices in group B
  }

  \item{empirical_connectance}{
    The target connectance for the network. Takes values from 0 to 0.5.
  }

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns an incidence matrix of the network.
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
