README.TXT
circa August 2021

This folder contains the files needed to create your research paper using the 2021 ACM conference style.

Using LaTex on Overleaf
—-------------—--------
If you are new to LaTeX, the easiest way to get started might be to to set up your own free account at overloaf.com, and upload the zip file. You will then need to edit the file, researchpaper.tex, and the file, researchpaper.bib. The first file contains the text of your paper, and the second file contains the bibliography of cited papers in BiBTeX style. Once both files are edited to your satisfaction, you can submit the generated pdf file.

However, if you do use Overleaf, it’s your responsibility to ensure that your account is kept secure and your files are kept private so that nobody can access the account. Read the syllabus handout about the penalties for academic dishonesty.

Local LaTeX Installations
-------------------------
You will need to edit the file, researchpaper.tex, and the file, researchpaper.bib. The first file contains the text of your paper, and the second file contains the bibliography of cited papers in BiBTeX style. Once both files are edited to your satisfaction, you will need to compile them using LaTeX and BibTeX to generate the required PDF file needed for submission.
	latex researchpaper
	bibtex researchpaper
	latex researchpaper
	latex researchpaper

To generate PDF, you may use a dvi2pdf program or replace the above sequence by the following:
	pdflatex researchpaper
	bibtex researchpaper
	pdflatex researchpaper
	pdflatex researchpaper

The sequences shown above are critical to generating the correct paper with appropriate citations and cross-references.

Submit the generated researchpaper.pdf to the dropbox.

Notes: 
   1. For your convenience, the initial generated pdf has been copied to a separate file RESEARCHPAPER_INITIAL.PDF so that you can refer to it for formatting help and suggestions. Don't submit this file by mistake. 
   2. You can always re-download the researchpaper.zip file from myCourses.