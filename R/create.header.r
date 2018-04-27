#' create header
#'
#' creates the latex header
#' @param title report title, no default
#' @export
## * content
## ** declare
create.header <- function(title)
{
    header <- paste0("\\documentclass[10pt, a4paper]{article} \n",
                     "\\usepackage[utf8]{inputenc} \n",
                     "\\usepackage{graphicx} \n",
                     "\\usepackage{helvet} \n",
                     "\\usepackage{tabularx} \n",
                     "\\usepackage{hyperref} \n",
                     "\\usepackage{makecell} \n",
                     "\\usepackage[a4paper, total={170mm, 257mm}]{geometry}",
                     "\\hypersetup{ \n",
                     "colorlinks, \n",
                     "citecolor=black, \n",
                     "filecolor=black, \n",
                     "linkcolor=black, \n",
                     "urlcolor=black} \n",
                     "\\newcommand{\\rinline}[1]{SOMETHING WRONG WITH knitr} \n",
                     "\\renewcommand{\\familydefault}{\\sfdefault} \n",
                     "\\title{", title, "} \n",
                     "\\date{", .session_variables$today, "} \n",
                     "\\begin{document} \n",
                     "\\maketitle \n",
                     "\\addtocontents{toc}{\\protect\\hypertarget{toc}{}} \n",
                     "\\tableofcontents \n",
                     "\\pagebreak \n")
    return(header)
## * end    
}
