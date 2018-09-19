
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
SOURCE_DIR=$THIS_DIR/../../../data

TAD1=${1:?Specify target dir as first argument}
TARGET_DIR=${TAD1%/}   # no trailing slash

TRD1=${2:?Specify source soubtree as second argument. Write / if you want to restore all the forest.}
TRD2="/${TRD1#/}"
TREE_DIR="${TRD2%/}/"  # exactly one starting and one trailing slash
 
echo $TREE_DIR
 
ymkdir () {
  if [[ "/$1" = "$TREE_DIR"* ]]; then
    DIR=${1#"$TREE_DIR"}
    mkdir -p "$TARGET_DIR/$DIR"
  fi
}

ycp () {
 if [[ "/$2" = "$TREE_DIR"* ]]; then
    DFILE=${2#"$TREE_DIR"}
    TARGET_PATH="$TARGET_DIR/$DFILE" 
    
    if [ -f "$TARGET_PATH" ]; then
      echo "allready exists: " "$TARGET_PATH"
    else
      echo "create: " "$TARGET_PATH"
      cp "$SOURCE_DIR/$1" "$TARGET_PATH"
    fi
   fi
}

ymodtime () {
  if [[ "/$1" = "$TREE_DIR"* ]]; then
    DIR=${1#"$TREE_DIR"}
    touch -d "$2" "$TARGET_DIR/$DIR"
  fi  
}
                              

ymkdir ""
ymkdir "addit"
ymkdir "addit/hahasoftareinfo"
ymkdir "addit/hahasoftareinfo/test"
ycp "2018-09/002/addit/hahasoftareinfo/test/XoftwareInfoPairTest.java" "addit/hahasoftareinfo/test/XoftwareInfoPairTest.java"
ymodtime "addit/hahasoftareinfo/test/XoftwareInfoPairTest.java" "2018-09-12 09:16:24.4504891 UTC"
ycp "2018-09/002/addit/hahasoftareinfo/test/XoftwareInfoTest.java" "addit/hahasoftareinfo/test/XoftwareInfoTest.java"
ymodtime "addit/hahasoftareinfo/test/XoftwareInfoTest.java" "2018-09-12 09:16:24.4504891 UTC"
ymkdir "maintree"
ymkdir "maintree/NECHATI"
ycp "2018-02/001/maintree/NECHATI/nechati11.txt" "maintree/NECHATI/nechati11.txt"
ycp "2018-02/001/maintree/NECHATI/nechati22.txt" "maintree/NECHATI/nechati22.txt"
ymkdir "maintree/NOVY-ADRESAR EXEMPLAR 1"
ycp "2018-09/002/maintree/NOVY-ADRESAR EXEMPLAR 2/1-soubor" "maintree/NOVY-ADRESAR EXEMPLAR 1/1-soubor"
ycp "2018-09/002/maintree/NOVY-ADRESAR EXEMPLAR 2/2-soubor" "maintree/NOVY-ADRESAR EXEMPLAR 1/2-soubor"
ymkdir "maintree/NOVY-ADRESAR EXEMPLAR 2"
ycp "2018-09/002/maintree/NOVY-ADRESAR EXEMPLAR 2/1-soubor" "maintree/NOVY-ADRESAR EXEMPLAR 2/1-soubor"
ymodtime "maintree/NOVY-ADRESAR EXEMPLAR 2/1-soubor" "2018-09-12 09:16:24.4544917 UTC"
ycp "2018-09/002/maintree/NOVY-ADRESAR EXEMPLAR 2/2-soubor" "maintree/NOVY-ADRESAR EXEMPLAR 2/2-soubor"
ymodtime "maintree/NOVY-ADRESAR EXEMPLAR 2/2-soubor" "2018-09-12 09:16:24.4554926 UTC"
ymkdir "maintree/NOVÁČEK"
ycp "2018-09/002/maintree/NOVÁČEK/novace1.txt" "maintree/NOVÁČEK/novace1.txt"
ymodtime "maintree/NOVÁČEK/novace1.txt" "2018-09-12 09:16:24.4554926 UTC"
ymkdir "maintree/NOVÝ-ADRESÁŘ"
ymkdir "maintree/NOVÝ-ADRESÁŘ/PŘESUNOUTI-DO-NOVĚHO"
ycp "2018-02/001/maintree/PŘESUNOUTI-DO-NOVĚHO/přesunouti-s-adresarem-do-noveho-11" "maintree/NOVÝ-ADRESÁŘ/PŘESUNOUTI-DO-NOVĚHO/přesunouti-s-adresarem-do-noveho-11"
ycp "2018-02/001/maintree/PŘESUNOUTI-DO-NOVĚHO/přesunouti-s-adresarem-do-noveho-22" "maintree/NOVÝ-ADRESÁŘ/PŘESUNOUTI-DO-NOVĚHO/přesunouti-s-adresarem-do-noveho-22"
ycp "2018-02/001/maintree/PŘESUNOUTI-DO-NOVĚHO/přesunouti22" "maintree/NOVÝ-ADRESÁŘ/PŘESUNOUTI-DO-NOVĚHO/přesunouti22"
ymkdir "maintree/NOVÝ-ADRESÁŘ/ZKOPÍROVATII-DO-NOVĚHO"
ycp "2018-02/001/maintree/ZKOPÍROVATII-DO-NOVĚHO/zkopirovati-s-adresarem-do-noveho-11" "maintree/NOVÝ-ADRESÁŘ/ZKOPÍROVATII-DO-NOVĚHO/zkopirovati-s-adresarem-do-noveho-11"
ycp "2018-02/001/maintree/ZKOPÍROVATII-DO-NOVĚHO/zkopirovati-s-adresarem-do-noveho-22" "maintree/NOVÝ-ADRESÁŘ/ZKOPÍROVATII-DO-NOVĚHO/zkopirovati-s-adresarem-do-noveho-22"
ycp "2018-09/002/maintree/treti exemplar souboru" "maintree/NOVÝ-ADRESÁŘ/ddddruhy exemplar souboru"
ycp "2018-09/002/maintree/NOVÝ-ADRESÁŘ/novy2" "maintree/NOVÝ-ADRESÁŘ/novy2"
ymodtime "maintree/NOVÝ-ADRESÁŘ/novy2" "2018-09-12 09:16:24.4604963 UTC"
ycp "2018-09/002/maintree/NOVÝ-ADRESÁŘ/novy3" "maintree/NOVÝ-ADRESÁŘ/novy3"
ymodtime "maintree/NOVÝ-ADRESÁŘ/novy3" "2018-09-12 09:16:24.4604963 UTC"
ymkdir "maintree/NOVÝ-ADRESÁŘ/novy4"
ycp "2018-09/002/maintree/treti exemplar souboru" "maintree/NOVÝ-ADRESÁŘ/novy4/druhy exemplar souboru"
ycp "2018-09/002/maintree/treti exemplar souboru" "maintree/NOVÝ-ADRESÁŘ/prvni exemplar souboru"
ycp "2018-02/001/maintree/přesunout-do-noveho" "maintree/NOVÝ-ADRESÁŘ/přesunout-do-noveho"
ycp "2018-02/001/maintree/zkopirovat-do-nového" "maintree/NOVÝ-ADRESÁŘ/zkopirovat-do-nového"
ymkdir "maintree/PREJMENOVANO A NECO SEM PRIDANO"
ycp "2018-02/001/maintree/PREJMENOVATI A NECO SEM PRIDATI/prejmenovati-adresar-a-pridati-soubor" "maintree/PREJMENOVANO A NECO SEM PRIDANO/prejmenovati-adresar-a-pridati-soubor"
ycp "2018-09/002/maintree/PREJMENOVANO A NECO SEM PRIDANO/pridano do prejmenovane slozky" "maintree/PREJMENOVANO A NECO SEM PRIDANO/pridano do prejmenovane slozky"
ymodtime "maintree/PREJMENOVANO A NECO SEM PRIDANO/pridano do prejmenovane slozky" "2018-09-12 09:16:24.463498 UTC"
ymkdir "maintree/PŘEJMENOVÁNO"
ycp "2018-02/001/maintree/PŘEJMENOVATI/přejmenovati11" "maintree/PŘEJMENOVÁNO/přejmenovati11"
ycp "2018-02/001/maintree/PŘEJMENOVATI/přejmenovati22" "maintree/PŘEJMENOVÁNO/přejmenovati22"
ymkdir "maintree/SEM-SE-TO-PŘESUNE"
ymkdir "maintree/SEM-SE-TO-PŘESUNE/PŘESUNOUTI-DO-EXISTUJÍCÍHO"
ycp "2018-02/001/maintree/PŘESUNOUTI-DO-EXISTUJÍCÍHO/přesunouti-s-adresarem-do-existujiciho-22" "maintree/SEM-SE-TO-PŘESUNE/PŘESUNOUTI-DO-EXISTUJÍCÍHO/přesunouti-s-adresarem-do-existujiciho-22"
ycp "2018-02/001/maintree/PŘESUNOUTI-DO-EXISTUJÍCÍHO/přesunouti-s-adresarem-do-existujícího-11" "maintree/SEM-SE-TO-PŘESUNE/PŘESUNOUTI-DO-EXISTUJÍCÍHO/přesunouti-s-adresarem-do-existujícího-11"
ycp "2018-02/001/maintree/přesunout-do-existujiciho" "maintree/SEM-SE-TO-PŘESUNE/přesunout-do-existujiciho"
ycp "2018-02/001/maintree/SEM-SE-TO-PŘESUNE/sem-se-něco-přesune" "maintree/SEM-SE-TO-PŘESUNE/sem-se-něco-přesune"
ymkdir "maintree/SEM-SE-TO-ZKOPIRUJE"
ymkdir "maintree/SEM-SE-TO-ZKOPIRUJE/ZKOPÍIROVATI-DO-EXISTUJÍCÍHO"
ycp "2018-02/001/maintree/ZKOPÍIROVATI-DO-EXISTUJÍCÍHO/zkopirovati-s-adresarem-do-existujiciho-22" "maintree/SEM-SE-TO-ZKOPIRUJE/ZKOPÍIROVATI-DO-EXISTUJÍCÍHO/zkopirovati-s-adresarem-do-existujiciho-22"
ycp "2018-02/001/maintree/ZKOPÍIROVATI-DO-EXISTUJÍCÍHO/zkopirovati-s-adresarem-do-existujícího-11" "maintree/SEM-SE-TO-ZKOPIRUJE/ZKOPÍIROVATI-DO-EXISTUJÍCÍHO/zkopirovati-s-adresarem-do-existujícího-11"
ycp "2018-02/001/maintree/SEM-SE-TO-ZKOPIRUJE/sem-se-něco-zkopíruje" "maintree/SEM-SE-TO-ZKOPIRUJE/sem-se-něco-zkopíruje"
ycp "2018-02/001/maintree/zkopirovat-do-existujiciho" "maintree/SEM-SE-TO-ZKOPIRUJE/zkopirovat-do-existujiciho"
ycp "2018-02/001/maintree/bude-zastiňovat" "maintree/bude-zastíněn"
ycp "2018-02/001/maintree/nechat" "maintree/nechat"
ycp "2018-09/002/maintree/novy" "maintree/novy"
ymodtime "maintree/novy" "2018-09-12 09:16:24.4725047 UTC"
ycp "2018-09/002/maintree/přejmenovat-a-nahradit" "maintree/přejmenovat-a-nahradit"
ymodtime "maintree/přejmenovat-a-nahradit" "2018-09-12 09:16:24.4735058 UTC"
ycp "2018-02/001/maintree/přejmenovat-a-nahradit" "maintree/přejmenováno-a-nahrazeno"
ycp "2018-02/001/maintree/přejmenovat-souborinek" "maintree/přejmenováno-souborinek"
ycp "2018-09/002/maintree/soubor-nulove-delky" "maintree/soubor-nulove-delky"
ymodtime "maintree/soubor-nulove-delky" "2018-09-12 09:16:24.475507 UTC"
ycp "2018-09/002/maintree/treti exemplar souboru" "maintree/treti exemplar souboru"
ymodtime "maintree/treti exemplar souboru" "2018-09-12 09:16:24.475507 UTC"
ycp "2018-09/002/maintree/změnit" "maintree/změnit"
ymodtime "maintree/změnit" "2018-09-12 09:16:24.4765078 UTC"
