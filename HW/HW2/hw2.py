from glob import glob
import re
import csv
from stop_words import get_stop_words
from nltk import word_tokenize
from nltk import trigrams
from collections import Counter
from nltk.stem import PorterStemmer
pt = PorterStemmer()

## Creating a Document-Term Matrix ##

############
## Step 1 ##
############

## shelby dictionary
paths_shelby = glob("releases/shelby/*")
raw_shelby = {}
for i in paths_shelby:
	info = i.split("/")[2]
	file_info = {}
	file_info["day"] = info[0:2]
	file_info["month"] = info[2:5]
	file_info["year"] = info[5:9]
	file_info["author"] = "shelby"
	file_info["text"] = " ".join(open(i).read().split())
	## nesting release dictionary into full dictionary
	## naming this particular dictionary 'Shelby100' for example
	raw_shelby[info[9:].replace(".txt", "")] = file_info

## sessions dictionary
paths_sessions = glob("releases/sessions/*")
raw_sessions = {}
for i in paths_sessions:
	info = i.split("/")[2]
	file_info = {}
	file_info["day"] = info[0:2]
	file_info["month"] = info[2:5]
	file_info["year"] = info[5:9]
	file_info["author"] = "sessions"
	file_info["text"] = " ".join(open(i).read().split())
	## nesting release dictionary into full dictionary
	raw_sessions[info[9:].replace(".txt", "")] = file_info


############
## Step 2 ##
############

## function that will process any string of text
## so it will work for each press release and the stopwords
def process_string(s, process_stopwords, stopwords_list = []):
	s = re.sub(r"\W", " ", s) 
	s = s.lower() 
	s = word_tokenize(s) 
	s = [pt.stem(i) for i in s]
	if not process_stopwords:
		s = [i for i in s if i not in stopwords_list]
	return s

## loading stopwords and processing them
special_sw = ["shelby", "sessions", "richard", "jeff", "email", "press", "room", "member", "senate"]
stopwords = " ".join(get_stop_words('en') + special_sw)
stopwords = process_string(stopwords, True)

## processing each press release in the full dictionary
## output is a list where each element is a list of processed tokens
processed_shelby = [process_string(raw_shelby[i]["text"], False, stopwords) for i in raw_shelby.keys()]
processed_sessions = [process_string(raw_sessions[i]["text"], False, stopwords) for i in raw_sessions.keys()]

## Making corpus level list: unlisting the lists of lists
corpus_words = sum(processed_shelby + processed_sessions, [])

## Creating trigram object, then extracting tuples, from each release
## so tg_shelby/tg_sessions are lists of lists (where each element
## in the list corresponds to a press release)
tg_shelby = []
for release in processed_shelby:
	tg_shelby.append([tri for tri in trigrams(release)])

tg_sessions = []
for release in processed_sessions:
	tg_sessions.append([tri for tri in trigrams(release)])

## Making corpus level list.  "Unlisting" like in R
corpus_tg = sum(tg_shelby + tg_sessions, [])

############
## Step 3 ##
############

## makes a dictionary of word/trigram counts, and then grabs the
## 1000/500 with the highest counts
common_words = Counter(corpus_words).most_common(1000)
common_trigrams = Counter(corpus_tg).most_common(500)

###################
## Steps 4 and 5 ##
###################

## Iterate over the processed press releases above
## For each of the common words, count how many are in press release
## Write row of length 1000/500 to csv
with open("word_counts.csv", 'ab') as f:

	## extracting only words (not counts)
	words = [i[0] for i in common_words]
	
	## setting up csv
	writer = csv.writer(f)
	writer.writerow(["author"] + words)

	## counting
	for release in processed_shelby:
		row = [release.count(w) for w in words]
		writer.writerow(["shelby"] + row)

	for release in processed_sessions:
		row = [release.count(w) for w in words]
		writer.writerow(["sessions"] + row)


with open("trigram_counts.csv", 'ab') as f:

	## extracting only words (not counts)
	tg_words = [i[0] for i in common_trigrams]
	## making tuple of words easier to handle
	header = [".".join(i) for i in tg_words]
	
	## setting up csv
	writer = csv.writer(f)
	writer.writerow(["author"] + header)

	for release in tg_shelby:
		## comparing to the most common list
		row = [tg_release.count(tri) for tri in tg_words]
		writer.writerow(["shelby"] + row)

	for release in tg_sessions:
		## comparing to the most common list
		row = [tg_release.count(tri) for tri in tg_words]
		writer.writerow(["sessions"] + row)






