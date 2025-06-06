% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generation.R
\name{data_generation}
\alias{data_generation}
\alias{GlobalTechDataGenerator}
\title{Data Generation for GlobalTech Analytics}
\description{
Generate employee and project data for testing and demos

R6 class for generating synthetic employee and project data
}
\examples{
\dontrun{
# Initialize the data generator
generator <- GlobalTechDataGenerator$new()
employees <- generator$generate_employee_data(100)
projects <- generator$generate_project_data(10, employees)
}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GlobalTechDataGenerator-new}{\code{GlobalTechDataGenerator$new()}}
\item \href{#method-GlobalTechDataGenerator-generate_employee_data}{\code{GlobalTechDataGenerator$generate_employee_data()}}
\item \href{#method-GlobalTechDataGenerator-generate_project_data}{\code{GlobalTechDataGenerator$generate_project_data()}}
\item \href{#method-GlobalTechDataGenerator-save_data}{\code{GlobalTechDataGenerator$save_data()}}
\item \href{#method-GlobalTechDataGenerator-clone}{\code{GlobalTechDataGenerator$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobalTechDataGenerator-new"></a>}}
\if{latex}{\out{\hypertarget{method-GlobalTechDataGenerator-new}{}}}
\subsection{Method \code{new()}}{
Initialize the data generator
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobalTechDataGenerator$new(seed = 42)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{seed}}{Random seed for reproducibility
Generate comprehensive employee dataset}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobalTechDataGenerator-generate_employee_data"></a>}}
\if{latex}{\out{\hypertarget{method-GlobalTechDataGenerator-generate_employee_data}{}}}
\subsection{Method \code{generate_employee_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobalTechDataGenerator$generate_employee_data(n_employees = 15000)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n_employees}}{Number of employees to generate}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
tibble with employee data
Generate realistic project dataset
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobalTechDataGenerator-generate_project_data"></a>}}
\if{latex}{\out{\hypertarget{method-GlobalTechDataGenerator-generate_project_data}{}}}
\subsection{Method \code{generate_project_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobalTechDataGenerator$generate_project_data(
  n_projects = 500,
  employee_df = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n_projects}}{Number of projects to generate}

\item{\code{employee_df}}{Employee dataframe for team assignments (optional)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
tibble with project data
Save all generated data to CSV files
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobalTechDataGenerator-save_data"></a>}}
\if{latex}{\out{\hypertarget{method-GlobalTechDataGenerator-save_data}{}}}
\subsection{Method \code{save_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobalTechDataGenerator$save_data(employee_df, project_df, output_dir = "data")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{employee_df}}{Employee data dataframe}

\item{\code{project_df}}{Project data dataframe}

\item{\code{output_dir}}{Output directory path
Setup company organizational structure
Setup skill matrices by department}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobalTechDataGenerator-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GlobalTechDataGenerator-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobalTechDataGenerator$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
