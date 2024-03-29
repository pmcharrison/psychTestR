% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/i18n.R
\name{timeline}
\alias{timeline}
\title{Timeline}
\description{
Timelines are series of psychTestR test elements that chain
together to form a test.
They support internationalisation,
defining parallel series of test elements for the available languages.
}
\section{Creation}{

Timelines are created using \code{\link{new_timeline}}.
}

\section{Manipulation}{

Timelines can be combined with other timelines and with test elements
using \code{\link{c}}.
}

\section{Usage}{

Timelines are ultimately passed to \code{\link{make_test}}.
}

\section{Other methods}{

\code{x$get(language, i)} returns a list of test elements corresponding
to \code{language} as extracted from the timeline \code{x},
or, if \code{i} is not \code{NULL}, the ith such test element.

\code{x$drop_languages(drop)} removes support for a set of languages
from timeline \code{x}, where \code{drop} is the character vector
of languages to remove.
}

\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-timeline-new}{\code{timeline$new()}}
\item \href{#method-timeline-get}{\code{timeline$get()}}
\item \href{#method-timeline-drop_languages}{\code{timeline$drop_languages()}}
\item \href{#method-timeline-print}{\code{timeline$print()}}
\item \href{#method-timeline-clone}{\code{timeline$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-timeline-new"></a>}}
\if{latex}{\out{\hypertarget{method-timeline-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{timeline$new(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-timeline-get"></a>}}
\if{latex}{\out{\hypertarget{method-timeline-get}{}}}
\subsection{Method \code{get()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{timeline$get(language, i = NULL)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-timeline-drop_languages"></a>}}
\if{latex}{\out{\hypertarget{method-timeline-drop_languages}{}}}
\subsection{Method \code{drop_languages()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{timeline$drop_languages(drop)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-timeline-print"></a>}}
\if{latex}{\out{\hypertarget{method-timeline-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{timeline$print(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-timeline-clone"></a>}}
\if{latex}{\out{\hypertarget{method-timeline-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{timeline$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
