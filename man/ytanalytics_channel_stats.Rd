% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ytanalytics_channel_stats.R
\name{ytanalytics_channel_stats}
\alias{ytanalytics_channel_stats}
\title{YouTube Analytics API Wrapper}
\usage{
ytanalytics_channel_stats(
  token = NULL,
  startDate = Sys.Date() - 30,
  endDate = Sys.Date()
)
}
\arguments{
\item{token}{environment}

\item{startDate}{character string}

\item{endDate}{character string}
}
\value{
df data.frame
}
\description{
This function returns number of Views, Shares, Likes, Dislikes,
Comments, Subs Gained & Lost for the channel (Can adjust date range)
}
\examples{
ytanalytics_channel_stats(token_var, "2020-05-01", "2020-05-31")

}
