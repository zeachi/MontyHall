% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{determine_winner}
\alias{determine_winner}
\title{Determine if the final pick wins}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{final.pick}{An integer for the final chosen door (1, 2, or 3).}

\item{game}{A length-3 character vector with "goat"/"car"
(from \code{\link{create_game}}).}
}
\value{
A character string: \code{"WIN"} if it's a car,
or \code{"LOSE"} if it's a goat.
}
\description{
\code{determine_winner()} checks whether the final chosen door
has a car or a goat.
}
\details{
It compares the door index in \code{final.pick} to the
arrangement in \code{game}. If that door holds a car,
the result is "WIN". Otherwise it's "LOSE".
}
\examples{
  g <- create_game()
  determine_winner(final.pick = 2, g)

}
