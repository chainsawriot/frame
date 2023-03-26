.PHONY: render sim appen all

all: render appen

render: frame.bib intermediate/LDA.RDS intermediate/STM.RDS intermediate/KM.RDS intermediate/PCA.RDS intermediate/ANTMN.RDS intermediate/SEEDED.RDS intermediate/KEYATM.RDS intermediate/expert_accuracy.RDS intermediate/brms_mod.RDS
	Rscript -e "rmarkdown::render('frame_ica_ea.rmd')"
appen: intermediate/conf_mod.RDS
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
intermediate/SEEDED.RDS: intermediate/lemma_tokens.RDS
	Rscript 07_SEEDED.R
intermediate/KEYATM.RDS: intermediate/lemma_tokens.RDS
	Rscript 08_KEYATM.R
intermediate/human_accuracy.RDS:
	Rscript 09_human.R
intermediate/expert_accuracy.RDS:
	Rscript 10_expert.R
intermediate/brms_mod.RDS: intermediate/LDA.RDS intermediate/STM.RDS intermediate/KM.RDS intermediate/PCA.RDS intermediate/ANTMN.RDS intermediate/SEEDED.RDS intermediate/KEYATM.RDS intermediate/human_accuracy.RDS
	Rscript 11_regression.R
intermediate/lemma_tokens_sim300.RDS:
	Rscript 13_sim_samplesize.R
intermediate/LDA_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_LDA.R
intermediate/STM_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_STM.R
intermediate/KM_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_KM.R
intermediate/PCA_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_PCA.R
intermediate/ANTMN_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_ANTMN.R
intermediate/SEEDED_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_SEEDED.R
intermediate/KEYATM_sim2000.RDS: intermediate/lemma_tokens_sim300.RDS
	Rscript 14_sim_KEYATM.R
sim: intermediate/LDA_sim2000.RDS intermediate/STM_sim2000.RDS intermediate/KM_sim2000.RDS intermediate/PCA_sim2000.RDS intermediate/ANTMN_sim2000.RDS intermediate/SEEDED_sim2000.RDS intermediate/KEYATM_sim2000.RDS
	echo "Doing simuation, it takes ages. Come back tommorrow!"
