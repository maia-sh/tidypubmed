% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml_tidy_text.R
\name{xml_tidy_text}
\alias{xml_tidy_text}
\alias{xml_tidy_attr}
\title{Extract xml text in tidy format}
\usage{
xml_tidy_text(nodes, xpath, label, counter = "n", id = "pmid")

xml_tidy_attr(nodes, xpath, attr, label)
}
\arguments{
\item{nodes}{article node set with PMID names}

\item{xpath}{xpath expression}

\item{label}{column name, default is last part of xpath}

\item{counter}{name for counter column, default is `n`}

\item{id}{name for id column, default is `pmid`}

\item{attr}{xml attribute name}
}
\value{
A tibble
}
\description{
Extract xml text in tidy format
}
\section{Functions}{
\itemize{
\item \code{xml_tidy_attr}: Extract xml attribute in tidy format
}}

\examples{
aq <- pubmed_nodeset(aqc)
xml_tidy_text(aq, "//Author/LastName")
xml_tidy_text(aq, "//Chemical/NameOfSubstance", "chemical")
xml_tidy_text(aq, "//Reference//ArticleId[@IdType='pubmed']", "cited")
}
