\name{foo}
\alias{foo}
% I'm a comment
\title{The foo function}

\description{It doesn't do much}

\usage{
foo(x, y)
}

\arguments{
  \item{x}{A number}
  \item{y}{Another number}
}
\details{
  I just adds \code{x} and \code{y}, 
}
\value{\code{numeric}. The sum of \code{x} and \code{y}.}
\references{
  \href{http://en.wikipedia.org/wiki/Sum}{Sum}
}
\author{
  Anonymous
}
\note{
  Lorem ipsum \dots \R \emph{emp}, \strong{strong}, \bold{bold},
  \sQuote{single quotes}, \dQuote{double quotes}, \code{code},
  \preformatted{x <- 2 + 2}, \kbd{type this}, \samp{literal seq},
  \pkg{base}, \file{foo.txt}, \email{email@hostname},
  \url{http://cran.r-project.org/}, \var{foo}, \env{HOME},
  \option{-d}, \dfn{something new}, \acronym{GNU}.

  Escaped symbols: \\ \{ \} \% not comment. \\NotAMacro.
  
   \tabular{rlll}{
         [,1] \tab alpha   \tab numeric \tab A (ppb)\cr
         [,2] \tab bravo \tab integer \tab B \cr
         [,3] \tab charlie    \tab character \tab C \cr
       }
   \enumerate{
         \item delta
         \item echo
       }
   \itemize{
         \item foxtrot
         \item golf
       }
  
  \deqn{p(x; \mu, \sigma^2) = \frac{1}{\sigma \sqrt{2 \pi}} \exp \frac{-(x - \mu)^2}{2 \sigma}{%
             p(\mu; x) = 1/\sigma (2 \pi)^(-1/2)  exp( -(x - \mu)^2 / (2 \sigma)) }
       for \eqn{x = 0, 1, 2, \ldots}.
  
  \if{latex}{\out{\beta}}\ifelse{html}{\out{&beta;}}{beta}

#ifdef unix
Now windows
#endif
#ifndef windows
Using windows
#endif

}
\section{Misc}{
  Stuff.
}

\seealso{
  \code{\link{sum}}
}
\examples{
x <- 1
y <- 2
z <- foo(x, y)
\dontrun{plot(z)}
\dontshow{log(x)}
}
\keyword{arith}
