render: frame.bib intermediate/LDA.RDS intermediate/STM.RDS intermediate/KM.RDS intermediate/PCA.RDS intermediate/ANTMN.RDS intermediate/SEEDED.RDS intermediate/KEYATM.RDS intermediate/expert_accuracy.RDS intermediate/brms_mod.RDS
	Rscript -e "rmarkdown::render('frame_ica_ea.rmd')"
	Rscript -e "rmarkdown::render('appen.rmd')"
frame.bib:
	bibcon -b /home/chainsawriot/dev/dotfiles/bib.bib -o frame.bib frame_ica_ea.rmd
intermediate/lemma_tokens.RDS:
	Rscript 01_prep.R
intermediate/LDA.RDS: intermediate/lemma_tokens.RDS
	Rscript 02_LDA.R
intermediate/STM.RDS: intermediate/lemma_tokens.RDS
	Rscript 03_STM.R
intermediate/KM.RDS: intermediate/lemma_tokens.RDS
	Rscript 04_KM.R
intermediate/PCA.RDS: intermediate/lemma_tokens.RDS
	Rscript 05_PCA.R
intermediate/ANTMN.RDS: intermediate/lemma_tokens.RDS
	Rscript 06_ANTMN.R
intermediate/SEEDED.RDS intermediate/KEYATM.RDS: intermediate/lemma_tokens.RDS
	Rscript 07_dictionary_methods.R
intermediate/human_accuracy.RDS:
	Rscript 09_human.R
intermediate/expert_accuracy.RDS:
	Rscript 10_expert.R
intermediate/brms_mod.RDS: intermediate/LDA.RDS intermediate/STM.RDS intermediate/KM.RDS intermediate/PCA.RDS intermediate/ANTMN.RDS intermediate/SEEDED.RDS intermediate/KEYATM.RDS intermediate/human_accuracy.RDS
	Rscript 11_regression.R
