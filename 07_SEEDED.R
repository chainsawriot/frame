set.seed(1212121)
source("lib.R")

resp <- c("cause","blame","guilt*","solution","necessary","insufficient","adequate","inadequate","useful","improv*","exacerbat*","pragmati*","*effective","*efficien*","avert","mistake","fail*","solve","accus*")

human <- c("civilian","affect*","pain*","suffer*","happy","family","love","mother","child","people","hurt","private","friend*","personal","casualt*")

conflict <- c("war","struggl*","fight*","conflict*","crime","assault*","battle*","violent","clash*","winner","loser","attack*","disagreement","challenge","defeat","victory")

moral <- c("moral*","immoral*","justice","injustice","fair*","unfair*","humane","inhumane","exploit*","oppress*","*tolerance","freedom","liberty","equality","equitable","compassion*","heartless","loyal","disloyal","rights","sacred","sin")

econ <- c("success","succeed","yield","gain","benefit","loss*","inflation","economy","*employment","growth","collapse","financial","cost","depreciation","devaluation","bubble","currency ","exchange")

exp1 <- dictionary(list(resp = resp, human = human, conflict = conflict, moral = moral, econ = econ))

resp2 <- c("responsib*", "blam*", "solution*")
human2 <- c("personal*", "affect*")
conflict2 <- c("conflict*", "winner*", "loser*", "disagree*", "crisis*", "crises*")
moral2 <- c("god*", "moral*", "values*")
econ2 <- c("loss*", "damage*", "consequence*", "gain*", "cost*", "percent*")

exp2 <- dictionary(list(resp = resp2, human = human2, conflict = conflict2, moral = moral2, econ = econ2))

exp3 <- dictionary(list(resp = union(resp, resp2), human = union(human, human2), conflict = union(conflict, conflict2), moral = union(moral, moral2), econ = union(econ, econ2)))

normal_tokens <- readRDS(ipath("normal_tokens.RDS"))

lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))
frame_df <- readRDS(ipath("frame_df.RDS"))

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0), expert = c(1, 2, 3))

n_conditions <- nrow(conditions)
conditions$exp1 <- rep(list(exp1), n_conditions)
conditions$exp2 <- rep(list(exp2), n_conditions)
conditions$exp3 <- rep(list(exp3), n_conditions)

generic_sim("SEEDED", experiment_seeded, conditions = conditions)
