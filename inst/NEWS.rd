\name{NEWS}
\title{News for Package \pkg{tidyHeatmap}}

\section{Changes in version 1.7.0}{
	\itemize{
		\item the argument `.scale` is now deprecated in favour of `scale`, and it is non by default. After few requests from the user-base. This change change the meaning of old code! To help with this destructive behaviour we added a warning to inform the user. The application of such impactful change is an absolute exception, in the development of tdyHeatmap. 
	}}

\section{Changes in version 1.8.0}{
	\itemize{
		\item For operation from `ComplexHeatmap` that are outside the functional framework (draw functionalities), we implemented the `as_CXomplexHeatmap` function, that outputs a `ComplexHeatmap`. 
		\item We implemented the `+` operator, natively for tidyHeatmap. 
	}}
	
\section{Changes in version 1.8.1}{
	\itemize{
		\item Improved custom palettes for annotations (e.g. add_tile). Now it is possible to choose a custom circlize::colorRamp2 scale.
	}}