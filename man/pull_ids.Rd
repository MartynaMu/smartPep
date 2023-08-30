\name{pull_ids}
\alias{pull_ids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pull peptide sequences or genes identified per sample}
\description{The function fetches a list of peptides or genes identified per sample.}
\usage{
pull_ids(long_df, sample_col, sequence_col, gene_col, save)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long_df}{A data.frame from long_format() function}
  \item{sample_col}{A character string. Name of the column storing sample names}
  \item{sequence_col}{A character string. Name of the column storing peptide sequences}
  \item{gene_col}{A character string. Name of the column storing gene symbols}
  \item{save}{A boolean. Whether to save each sample list in .txt file}
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
