render:
	Rscript -e "rmarkdown::render('frame_ica_ea.rmd')"
bib:
	bibcon -b /home/chainsawriot/dev/dotfiles/bib.bib -o frame.bib frame_ica_ea.rmd
