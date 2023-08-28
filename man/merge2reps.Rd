\name{merge2reps}
\alias{merge2reps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merge technical duplicates.
%%  ~~function to do ... ~~
}
\description{Merges two technical replicates by averaging two measurements and imputing the NAs with the neighbour.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
merge2reps(wide_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wide_df}{A data.frame of peptide intensities in wide format}
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
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
