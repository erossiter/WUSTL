import re
import csv
from stop_words import get_stop_words
from nltk import word_tokenize
from collections import Counter
from nltk.stem import PorterStemmer
pt = PorterStemmer()
import glob


###################################
## Part 2 : Machiavelli's Prince ##
###################################

## Reading in Mach files
mach_files = glob.glob("MachText/*.txt")

mach = []
for i in mach_files:
	with open(i) as f:
		mach.append(f.readlines())


def process_string(s, process_stopwords, stopwords_list = []):
	s = re.sub(r'[^\w\s]','',s)
	s = s.lower() 
	s = word_tokenize(s) 
	s = [pt.stem(i) for i in s]
	if not process_stopwords:
		s = [i for i in s if i not in stopwords_list]
	return s

## processing stopwords
stopwords = " ".join(get_stop_words('en'))
stopwords = process_string(stopwords, True)

## a list of lists (each element a processed section of the text)
processed_mach = [process_string("".join(i), False, stopwords) for i in mach]

## Making corpus level list: unlisting the lists of lists
corpus_words = sum(processed_mach, [])

## Making list of 500 most used terms
common_words = dict(Counter(corpus_words).most_common(500))

with open("doc_term_mat.csv", 'ab') as f:

	## extracting only words (not counts)
	words = common_words.keys()
	
	## setting up csv
	writer = csv.writer(f)
	writer.writerow(["file"] + words)

	## counting
	for i, section in enumerate(processed_mach):
		row = [section.count(w) for w in words]
		writer.writerow([mach_files[i]] + row)


## moving back over to R ##

















