\name{binding_category}
\alias{binding_category}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Categorize NetMHCpan 4.1b results
%%  ~~function to do ... ~~
}
\description{This function takes a cleaned-up .xlsx file from NetMHCpan 4.1 binding affinity results including BA predictions and exchanges the numerical prediction data into 4 categories for each HLA allele predicted - "Strong-binder"/"Weak-binder"/"Non-binder"/"Binder".
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binding_category(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A cleaned-up data.frame from cleanup_ranks() function}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{A data-frame of the same size as cleaned-up data-frame from cleanup_ranks() function.
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
