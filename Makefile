.PRECIOUS: data/processed/l2_genus_%.Rds  data/processed/rf_genus_%.Rds 

data/raw/baxter.% : 
	wget https://github.com/riffomonas/minimalR-raw_data/archive/refs/tags/0.3.zip
	unzip 0.3.zip -d data/
	mv data/minimalR-raw_data-0.3 data/raw
	rm 0.3.zip
	
SEEDS=$(shell seq 1 1 10)
L2_GENUS_RDS=$(patsubst %,data/processed/l2_genus_%.Rds,$(SEEDS))

$(L2_GENUS_RDS) : code/run_split.R\
code/genus_process.R\
code/l2_genus.R\
data/raw/baxter.subsample.shared\
data/raw/baxter.cons.taxonomy\
data/raw/baxter.metadata.tsv
	$< $@ "code/l2_genus.R"
	
data/processed/l2_genus_pooled_%.tsv : code/combine_models.R $(L2_GENUS_RDS)
	$^ $@
	
L2_FIT_RDS=$(patsubst %,data/processed/l2_genus_fit_%.Rds,$(SEEDS))	

$(L2_FIT_RDS) : code/run_split.R\
code/genus_process.R\
code/l2_genus_fit.R\
data/raw/baxter.subsample.shared\
data/raw/baxter.cons.taxonomy\
data/raw/baxter.metadata.tsv
	$< $@ "code/l2_genus_fit.R"
	
data/processed/l2_fit_genus_pooled_%.tsv : code/combine_models.R $(L2_FIT_RDS)
	$^ $@
	
RF_GENUS_RDS=$(patsubst %,data/processed/rf_genus_%.Rds,$(SEEDS))

$(RF_GENUS_RDS) : code/run_split.R\
code/genus_process.R\
code/rf_genus.R\
data/raw/baxter.subsample.shared\
data/raw/baxter.cons.taxonomy\
data/raw/baxter.metadata.tsv
	$< $@ "code/rf_genus.R"
	
data/processed/rf_genus_pooled_%.tsv : code/combine_models.R $(RF_GENUS_RDS)
	$^ $@
	
RF_FIT_RDS=$(patsubst %,data/processed/rf_genus_fit_%.Rds,$(SEEDS))	

$(RF_FIT_RDS) : code/run_split.R\
code/genus_process.R\
code/rf_genus_fit.R\
data/raw/baxter.subsample.shared\
data/raw/baxter.cons.taxonomy\
data/raw/baxter.metadata.tsv
	$< $@ "code/rf_genus_fit.R"
	
data/processed/rf_fit_genus_pooled_%.tsv : code/combine_models.R $(RF_FIT_RDS)
	$^ $@