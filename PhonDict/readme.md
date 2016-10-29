# Document Use of Subset\_CocaMoby.py for Derivation of OtoP Dictionary

This file documents use of Subset\_CocaMoby.py for the purpose of
building a training dictionary for use in an OtoP learning simulation
built in MikeNet. Dictionary entries, ortho/phono pairs together with
frequency weights, are derived from the CoCa and Moby
databases. Frequency information comes from CoCa
<http://corpus.byu.edu/coca/>, and O-to-P mappings from the Moby
Pronunciator <http://icon.shef.ac.uk/Moby/>.

Words selected for inclusion in the training dictionary are
monosyllabic and (mostly) mono-morphemic.

We incorporate some simple transformations to the Moby pronunciations
in order to facilitate use in a Mikenet framework.

## Details of steps from Moby/CoCa to MikeNet OtoP Dictionary
There are two types of phonological representations following respectively 
Harm & Seidenberg (1999) and Harm (1998). The two papers and other relevant
ones are stored respectively in the folders ./HarmSeidenberg1999 and ./Harm1998.
In those folders, phon\_HarmSeidenberg1999.txt and phon\_Harm1998.txt are the dictionary
of phonemes to be used by the code. The explanantions of those phonemes can be found 
in phon\_HarmSeidenberg1999_key.txt and phon\_Harm1998_key.txt.

Compared with the two sets of phoneme dictionaries, the sets under Harm1998 are kinda preferred, due to their binary settings of phonemes and good training performances.

Below are the details of steps from Moby/Coca to MikeNet OtoP Dictionary following
Harm & Seidenber (1999)'s way of phonological representations.

1. Read from Moby pronunciator and make some replacements in the phones.
   These replacements include:
   1. Delete the glide '/-/' after the vowel '/aI/'. This mostly
      occurs before /r/, as in 'fire'.
   2. Replace '/-/' with '/@/'. Moby uses /-/ to flag syllabic liquid
      consonants as in 'battle' or 'butter'. We replace these with
      straight up schwa-C sequences to conform to phonological
      inventory of our mikenet simulations.
   3. Replace '/hw/' with 'w'. The voiceless glide is not available in
      our current mikenet inventory, and is not contrastive in
      stereotypical American English.
   4. Replace 'R' with 'r'.
   5. Replace '/(@)/' and '/[@]/' with '/@/'.
   6. Remove words containing phone '/x/' (Bach, loch).

2. Read modified Moby lexicon after 1) and select only those words
   having at most 1 vowel (1 syllable).  The results are stored in
   subMobypron\_dict.csv.

3. Read Coca database and select words having at most 8 letters. There
   are two result CSV files based on different Coca database:
   subCoca\_dict1.csv is based on the lemma words' frequencies;
   subCoca\_dict2.csv is based on real word forms' frequencies;

4. Remove slashes from Moby pronunciations. Also replace digraph
   (polygraph) symbols with monographs. Replacements includes:
   1. For consonants and simple vowels;
	  * consonants:
		* '/tS/'->'C'
		* '/dZ/'->'J'
		* '/S/'->'S'
		* '/T/'->'T'
		* '/D/'->'D'
		* '/Z/'->'B'
		* '/N/'->'G'

	  * simple vowels:
		* '/i/'->'i'
		* '/I/'->'I'
		* '/E/'->'E'
		* '/&/'->'@'
		* '/A/'->'a'
		* '/O/'->'a'
		* '/(@)/'->'E'
		* '/oU/'->'o'
		* '/U/'->'U'
		* '/u/'->'u'
		* '/@/'->'^'

   2. Separate diphthongs into single phone into two phone sequence
      consisting of a simple vowel plus consonantal glide:
	  * '/eI/'->'e/j'
	  * '/aI/'->'a/j'
	  * '/Oi/'->'o/j'
	  * '/AU/'->'a/w'

   Results are stored respectively in extwords1\_HarmSeidenberg1999.csv 
   (based on subCoca\_dict1.csv) and extwords2\_HarmSeidenberg1999.csv 
   (based on subCoca\_dict2.csv)

   In extword1\_HarmSeidenberg1999.csv and extword2\_HarmSeidenberg1999.csv, 
   column 'wordform' is word form, column 'sum\_freq' is word frequency in Coca, 
   column 'norm\_freq' is normalized frequency per million (dividing 'sum\_freq' by 450),
   column 'Moby\_pron' is Moby pronunciation of the word, column 'Rep\_P' is 
   phonological representation of the word, column 'Rep\_O' is orthographical 
   representation of the word, '/' separates phonemes or letters.
   In extword2.csv, column 'word\_class' is class type of each word.

5. Create extword3\_HarmSeidenberg1999.csv based on extword2\_HarmSeidenberg1999.csv, 
  by summing up the same word form's frequencies across all word types.

6. Generate training example files based on extword1\_HarmSeidenberg1999.csv, 
   extword2\_HarmSeidenberg1999.csv and extword3.csv, the results are: TrEm1\_HarmSeidenberg1999.txt,
   TrEm2\_HarmSeidenberg1999.txt, and TrEm3\_HarmSeidenberg1999.txt. The csv and txt files are in folder
   ./HarmSeidenberg1999.

Following Harm (1998)'s way of phonological representation, the above Steps 1-3 are the same.
In Step 4, for the replacements, consonants and simple vowels are the same, but the diphthongs become:
	* '/eI/'->'e'
	* '/aI/'->'Y'
	* '/Oi/'->'A'
	* '/AU/'->'O'
After replacements, one also needs to replace combined phomenes '/j/u/' to '/W/'.  

In Steps 5 and 6, the created csv files become: extword1\_Harm1998.csv, extword2\_Harm1998.csv, 
and extword1\_Harm1998.csv. The created training example files become: TrEm1\_HarmSeidenberg1999.txt,
TrEm2\_HarmSeidenberg1999.txt, and TrEm3\_HarmSeidenberg1999.txt. The csv and txt files are in folder ./Harm1998.

Due to coding error of Moby database, we correct some errors in extword3\_Harm1998.csv, see extword3\_Harm1998\_correct.txt and extword4\_Harm1998.txt. Accordingly,  the final training example file is TrEm4\_Harm1998.txt.

Apart from these example files used for OtoP training, we also created example files used for PtoP training (e.g., TrEm4\_PtoP\_Harm1998.txt. The differences between the two types of example files are:
  * In OtoP training example files, the clamped input is in ortho layer, whereas in PtoP training example files, the clamped input in also in phono layer.
  * In OtoP training example files, the clamped input is in ticks 0 to 6, and the target output is in ticks 4 to 6 (giving target early can speed up the learning process, see Harm & Seidenberg 2004). By contrast, in PtoP training example files, the clamped input is in tick 0, and the target output is in ticks 2-4. The purpose of such training is to train the cleanup units to hold the input through ticks.

In addition to these phonemes and training examples. There is a folder ./examples, in which there are two subfolders called nonwords and realwords. In the subfolder nonwords, we provide one set of nonwords for testing the performance of the model. The Te.txt file is the text file which can be used as the testing example of the model. The nonwords examples in this file are extracted from the excel and csv files, which are taken from the Appendix of the paper by Treiman et al. (1990). The other subfolder realwords contains some excel files, the real words in which are taken from several papers, including Strain et al. (1995) Appendices A and B (these examples address the interaction between frequency and regularity of words), Strain et al. (2002) (these examples also address the effect of regularity), and Taraba & McClelland (1987) Appendices A1 and A2 (these examples address the interaction between frequency and regularity (consistency)). All these examples are used to test whether the model can report similar results to the previous empirical findings.   