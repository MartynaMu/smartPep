\name{plot_binder_shares}
\alias{plot_binder_shares}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot shares of binder peptide per MHC allele
%%  ~~function to do ... ~~
}
\description{
Plots two bar graphs of shares of binding affinity categories per allele and globally.
}
\usage{
plot_binder_shares(ba_long, MHC_col, category_col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ba_long}{A data.frame acquired after `binding_category()` function pivoted to long format}
  \item{MHC_col}{A character string. Name of the column containing names of MHC alleles}
  \item{category_col}{A character string. Name of the column containing binding categories}
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
