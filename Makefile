.PHONY: render simulate appen all

all: render appen

render: frame.bib intermediate/LDA.RDS intermediate/STM.RDS intermediate/KM.RDS intermediate/PCA.RDS intermediate/ANTMN.RDS intermediate/SEEDED.RDS intermediate/KEYATM.RDS intermediate/expert_accuracy.RDS 
	Rscript -e "rmarkdown::render('manuscript.rmd')"
	Rscript -e "rmarkdown::render('manuscript.rmd', output_format = 'papaja::apa6_word')"
appen: intermediate/conf_mod.RDS intermediate/sim/brms_mod_sim2000.RDS intermediate/expert_accuracy_k4.RDS intermediate/human_tau.RDS
	Rscript -e "rmarkdown::render('appen.rmd')"
frame.bib:
	bibcon -b /home/chainsawriot/dev/dotfiles/bib.bib -o frame.bib manuscript.rmd
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
intermediate/sim/lemma_tokens_sim2000.RDS:
	Rscript 13_sim_samplesize.R
intermediate/sim/LDA_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_LDA.R
intermediate/sim/STM_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_STM.R
intermediate/sim/KM_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_KM.R
intermediate/sim/PCA_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_PCA.R
intermediate/sim/ANTMN_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_ANTMN.R
intermediate/sim/SEEDED_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_SEEDED.R
intermediate/sim/KEYATM_sim2000.RDS: intermediate/sim/lemma_tokens_sim2000.RDS
	Rscript 14_sim_KEYATM.R
intermediate/sim/brms_mod_sim2000.RDS: intermediate/sim/LDA_sim2000.RDS intermediate/sim/STM_sim2000.RDS intermediate/sim/KM_sim2000.RDS intermediate/sim/PCA_sim2000.RDS intermediate/sim/ANTMN_sim2000.RDS intermediate/sim/SEEDED_sim2000.RDS intermediate/sim/KEYATM_sim2000.RDS
	echo "Simulation done. It took ages."
	Rscript 15_sim_brms.R
intermediate/expert_accuracy_k4.RDS:
	Rscript 18_human_k4.R
intermediate/human_tau.RDS:
	Rscript 20_tau.R
