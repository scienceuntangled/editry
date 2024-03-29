% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exec.R
\name{er_exec_docker}
\alias{er_exec_docker}
\title{Execute editly in docker image}
\usage{
er_exec_docker(
  spec,
  docker_image = "scienceuntangled/editly",
  fast = FALSE,
  extra_docker_args
)
}
\arguments{
\item{spec}{er_spec: an \code{er_spec} object as returned by \code{\link[=er_spec]{er_spec()}}}

\item{docker_image}{string: name of the docker image to use}

\item{fast}{logical: if \code{TRUE}, run editly in 'fast' (low quality/preview) mode}

\item{extra_docker_args}{character: additional arguments to pass to the \code{docker} command, e.g. \code{extra_docker_args = c("--cpu-shares", "512")} will limit the docker containiner's CPU priority when CPU cycles are limited (see \url{https://docs.docker.com/config/containers/resource_constraints/})}
}
\value{
0 for success (invisibly)
}
\description{
Does not require node or editly to be installed on your system, instead requires that Docker is running and that you have pulled a suitable editly docker image. By default this is \code{scienceuntangled/editly}, which uses a modified version of \code{editly}. Other \code{editly} docker images can be used, but will not support the \verb{logo_*} parameters in \code{er_header} or \code{er_spec}.
}
\examples{
\dontrun{
  clips <- list(er_clip_title2(duration = 3, text = "Title slide"),
                er_clip_image(duration = 3, path = "https://jeroen.github.io/images/Rlogo.png"))
  outfile <- tempfile(fileext = ".mp4")
  my_spec <- er_spec(clips = clips , out_path = outfile, allow_remote_requests = TRUE)
  er_exec_docker(my_spec)
  if (interactive()) browseURL(outfile)
}

}
\references{
\url{https://github.com/mifi/editly/}
}
