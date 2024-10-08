% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host opens a door, revealing a goat.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{This}{function uses arguments 'game' and 'a.pick' which include the game setup created from create_game that randomly
establishes which doors contain which items (2 goats, 1 car) and the contestants initiall selection from select_door
that is used to isolate what doors are available to the host to pick from}
}
\value{
This function returns a single numeric value representing which door the host chose to open, based on the criteria described
}
\description{
The host, after the contestants initial pick, reveals one of the doors that has a goat behind it. This door
may not be the door the contestant selected, and of course, must not be the door with the car behind it. The
door must reveal a goat.
}
\details{
After the contestant makes a selection, the host must open one of the remaining doors that has a goat behind it.
The host must not pick the door the contestant picked, and must also only pick a door containing a goat. By reducing
the hosts qualified options, the host is able to randomly select between the 2 remaining goat doors - assuming the
contestant selected the car to begin with, or the single goat door left if the contestant picked one of the two goat
doors to begin with.
}
\examples{
game <- create_game()
a.pick <- select_door()
open_goat_door(game, a.pick)
}
