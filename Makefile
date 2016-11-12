all: report.pdf

report.pdf: report/*.Rmd
	cat report/*.Rmd > report/Report.Rmd
	Rscript -e "library(rmarkdown); render('report/Report.Rmd', 'pdf_document')"
clean:
	rm -f report/Report.Rmd
	rm -f report/Report.pdf
