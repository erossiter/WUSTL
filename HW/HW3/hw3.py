import re
import csv
from stop_words import get_stop_words
from nltk import word_tokenize
from collections import Counter
from nltk.stem import PorterStemmer
pt = PorterStemmer()
import json


#########################################################
## Part 1 : Preprocessing and Creating Doc Term Matrix ##
#########################################################

## reading in data
with open("nyt_ac.json") as json_data:
    nyt_data = json.load(json_data)

## writing pertinent text to files
for i, doc in enumerate(nyt_data):
	title = doc["body"]["title"]
	text = doc["body"]["body_text"]
	with open("stories/doc" + str(i) + ".txt", "w") as f:
		f.write(title.encode("UTF-8"))
		f.write("\n")
		f.write(text.encode("UTF-8"))

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

## a list of lists (each element a processed story)
processed_nyt = [process_string(i["body"]["body_text"], False, stopwords) for i in nyt_data]

## grabbing desks
desks = [i["meta"]["dsk"]for i in nyt_data]

## a check
len(processed_nyt) == len(desks)

## Making corpus level list: unlisting the lists of lists
corpus_words = sum(processed_nyt, [])

## Making list of 1000 most used terms
common_words = Counter(corpus_words).most_common(1000)

with open("doc_term_mat.csv", 'ab') as f:

	## extracting only words (not counts)
	words = [i[0] for i in common_words]
	
	## setting up csv
	writer = csv.writer(f)
	writer.writerow(["desk"] + words)

	## counting
	for i, story in enumerate(processed_nyt):
		row = [story.count(w) for w in words]
		writer.writerow([desks[i]] + row)


## Clustering Methods part of exercise is in R script



################################################
## Part 2 : Dictionary Classification Methods ##
################################################

## Loading positive and negative words
pos = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/positive.txt").read()
pos = pos.split("\n")

neg = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/negative.txt").read()
neg = neg.split("\n")


###################################################
## Part 3 : Supervised Learning with Naive Bayes ##
###################################################



