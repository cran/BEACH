\name{runBEACH}
\alias{runBEACH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run the BEACH app
}
\description{
  The web interface will be automatically load after running the function.
}
\usage{
runBEACH()
}
%- maybe also 'usage' for other objects documented here.
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Danni Yu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
  #Install a github package that is capable to import compressed sas7bdat files.
  #if(!"sas7bdat.parso" %in% installed.packages()) 
  #  devtools::install_github('BioStatMatt/sas7bdat.parso', force=TRUE)
  #
  #runBEACH() 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
