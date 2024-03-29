% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dscore-package.R
\docType{package}
\name{dscore-package}
\alias{dscore-package}
\title{dscore: D-score for Child Development}
\description{
The \code{dscore} package implements several tools needed to
calculate the D-score, a numerical score that measures
generic development in children.
}
\note{
This study was supported by the Bill & Melinda Gates Foundation.
The contents are the sole responsibility of the authors and may not
necessarily represent the official views of the Bill & Melinda
Gates Foundation or other agencies that may have supported the
primary data studies used in the present study.

The authors wish to
recognize the principal investigators and their study team members
for their generous contribution of the data that made this tool
possible and the members of the Ki team who directly or indirectly
contributed to the study: Amina Abubakar, Claudia R. Lindgren
Alves, Orazio Attanasio, Maureen M. Black, Maria Caridad Araujo,
Susan M. Chang-Lopez, Gary L. Darmstadt, Bernice M. Doove, Wafaie
Fawzi, Lia C.H. Fernald, Günther Fink, Emanuela Galasso, Melissa
Gladstone, Sally M. Grantham-McGregor, Cristina Gutierrez de
Pineres, Pamela Jervis, Jena Derakhshani Hamadani, Charlotte
Hanlon, Simone M. Karam, Gillian Lancaster, Betzy Lozoff, Gareth
McCray, Jeffrey R Measelle, Girmay Medhin, Ana M. B. Menezes,
Lauren Pisani, Helen Pitchik, Muneera Rasheed, Lisy
Ratsifandrihamanana, Sarah Reynolds, Linda Richter, Marta
Rubio-Codina, Norbert Schady, Limbika Sengani, Chris Sudfeld,
Marcus Waldman, Susan P. Walker, Ann M. Weber and Aisha K.
Yousafzai.
}
\section{User functions}{

The available functions are:\tabular{ll}{
   Function \tab Description \cr
   \code{\link[=get_itemnames]{get_itemnames()}} \tab Extract item names from an itemtable \cr
   \code{\link[=order_itemnames]{order_itemnames()}} \tab Order item names \cr
   \code{\link[=sort_itemnames]{sort_itemnames()}} \tab Sort item names \cr
   \code{\link[=decompose_itemnames]{decompose_itemnames()}} \tab Get four components from itemname \cr
     \tab  \cr
   \code{\link[=get_itemtable]{get_itemtable()}} \tab Get a subset from the itemtable \cr
   \code{\link[=get_labels]{get_labels()}} \tab Get labels for items \cr
   \code{\link[=rename_gcdg_gsed]{rename_gcdg_gsed()}} \tab Rename gcdg into gsed lexicon \cr
     \tab  \cr
   \code{\link[=dscore]{dscore()}} \tab Estimate D-score and DAZ \cr
   \code{\link[=dscore_posterior]{dscore_posterior()}} \tab Calculate full posterior of D-score \cr
   \code{\link[=get_tau]{get_tau()}} \tab Get difficulty parameters from item bank \cr
     \tab  \cr
   \code{\link[=daz]{daz()}} \tab Transform to age-adjusted standardized D-score \cr
   \code{\link[=zad]{zad()}} \tab Inverse of \code{\link[=daz]{daz()}} \cr
   \code{\link[=get_reference]{get_reference()}} \tab Get D-score age-reference \cr
   \code{\link[=get_age_equivalent]{get_age_equivalent()}} \tab Translate difficulty to age \cr
}
}

\section{Built-in data}{

The package contains the following built-in data:\tabular{ll}{
   Data \tab Description \cr
   \code{\link[=builtin_itembank]{builtin_itembank()}} \tab A \code{data.frame} containing the difficulty estimates of items according to final Rasch models. \cr
   \code{\link[=builtin_itemtable]{builtin_itemtable()}} \tab A \code{data.frame} containing names and descriptions of items from 22 instruments. \cr
   \code{\link[=builtin_references]{builtin_references()}} \tab A \code{data.frame} with LMS reference values used to transform from D-score to DAZ, DAZ to D-score. \cr
   \code{\link[=milestones]{milestones()}} \tab A small demo dataset with PASS/FAIL responses from 27 preterms, measured at various ages between birth \cr
   and 2.5 years. \tab  \cr
}
}

\references{
Jacobusse, G., S. van Buuren, and P.H. Verkerk. 2006. “An Interval Scale
for Development of Children Aged 0-2 Years.” \emph{Statistics in Medicine} 25 (13):
2272–83. \url{https://stefvanbuuren.name/publication/jacobusse-2006/}

Van Buuren S (2014). Growth charts of human development.
Stat Methods Med Res, 23(4), 346-368.
\url{https://stefvanbuuren.name/publication/van-buuren-2014-gc/}

Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
Richter L, Black MM (2019). The D-score: a metric for interpreting
the early development of infants and toddlers across global settings.
BMJ Global Health, BMJ Global Health 4: e001724.
\url{https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf}.

GSED team (Maureen Black, Kieran Bromley, Vanessa Cavallera
(lead author), Jorge Cuartas, Tarun Dua (corresponding author),
Iris Eekhout, Gunther Fink, Melissa Gladstone, Katelyn Hepworth,
Magdalena Janus, Patricia Kariger, Gillian Lancaster, Dana McCoy,
Gareth McCray, Abbie Raikes, Marta Rubio-Codina, Stef van Buuren,
Marcus Waldman, Susan Walker and Ann Weber). 2019. “The Global Scale
for Early Development (GSED).” \emph{Early Childhood Matters}.
\url{https://earlychildhoodmatters.online/2019/the-global-scale-for-early-development-gsed/}
}
