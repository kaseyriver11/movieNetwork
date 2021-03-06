% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_network_data.R
\name{create_link_df}
\alias{create_link_df}
\title{create pairwise name combinations for each movie}
\usage{
create_link_df(df, cast_crew_select, k = 20)
}
\arguments{
\item{df}{data frame created by create_network_df function}

\item{cast_crew_select}{value of 'cast' or 'crew' dependent on whether the user wanted a cast network or crew network}

\item{k}{The Number of cast or crew to take. We make this an option because it can be computationally
expensive to make connections between a long list of cast or crew members}
}
\value{
a dataframe of each combination of cast and crew names
}
\description{
Take the data frame created by create_network_df and return all
 combinations of actors or cast within each movie
}
