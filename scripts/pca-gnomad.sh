#!/usr/bin/env bash


# Parameters

tempFolder=/mnt/archive/mody/tmp/admixture


# Functions

# Create param file for convertf
# Taken from https://github.com/ModyX/erc-genotypes/blob/master/lib/functions.sh.
#
# Arg1 Input path + stem (og ped/map fileset)
# Arg2 Output path + stem
# Arg3 Output path + filename of convertf param file
function create_convertf_paramfile {
    echo "genotypename:    $1.ped" > $3
    echo "snpname:         $1.pedsnp" >> $3
    echo "indivname:       $1.pedind" >> $3
    echo "outputformat:    EIGENSTRAT" >> $3
    echo "genotypeoutname: $2.geno" >> $3
    echo "snpoutname:      $2.snp" >> $3
    echo "indivoutname:    $2.ind" >> $3
    echo "familynames:     YES" >> $3
}

# Create param file for smartpca
# Taken from https://github.com/ModyX/erc-genotypes/blob/master/lib/functions.sh.
#
# Arg1 Input path + stem (og ped/map fileset)
# Arg2 Output path + stem
# Arg3 Path to the param file
function create_smartpca_paramfile {
	echo "genotypename:	$1.geno" > $3
	echo "snpname:		$1.snp" >> $3
	echo "indivname:	$1.ind" >> $3
	echo "evecoutname:	$2.pca.evec" >> $3
	echo "evaloutname:	$2.eval" >> $3
	echo "altnormstyle:	NO" >> $3
	echo "numoutevec:	10" >> $3
	echo "numoutlieriter:	0" >> $3
	echo "numoutlierevec:	10" >> $3
	echo "outliersigmathresh: 6" >> $3
	echo "qtmode:		0" >> $3
	echo "fastmode:		YES" >> $3
}



# Folders

workDir=/mnt/work/marc/nordic/pca
plinkDir=/mnt/work/marc/tools/plink1.90b3.36

# Create plink files
$plinkDir/plink \
	--bcf /mnt/archive/resources/gnomad/r2.1/gnomad.exomes.r2.1.sites.vcf.bgz
	--make-bed \
	--out $workDir/genomes




