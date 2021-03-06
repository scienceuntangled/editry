% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{er_install_editly}
\alias{er_install_editly}
\title{Install the editly node package}
\usage{
er_install_editly(force = FALSE, dependencies = TRUE)
}
\arguments{
\item{force}{logical: if \code{TRUE}, force reinstallation of editly if it is already installed}

\item{dependencies}{logical: if \code{TRUE}, the system binaries \code{node} (via \code{\link[noder:nr_install_node]{noder::nr_install_node()}}) and \code{ffmpeg} (via \code{\link[=er_install_ffmpeg]{er_install_ffmpeg()}}) will also be installed if not found on your system}
}
\value{
The path to the installation (invisibly)
}
\description{
Install the editly node package
}
\references{
\url{https://github.com/mifi/editly/}
}
