from bs4 import BeautifulSoup
import urllib2 
import re
import csv
from stop_words import get_stop_words
from nltk import word_tokenize
from nltk.stem.lancaster import LancasterStemmer 
from nltk.stem import PorterStemmer 
from nltk.stem.snowball import EnglishStemmer
st = LancasterStemmer()
pt = PorterStemmer()
sb = EnglishStemmer()


###############
## Problem 1 ##
###############

## Reading in html 
web_page = urllib2.urlopen("file:///Users/erinrossiter/Dropbox/Github/WUSTL/Debate1.html")
soup = BeautifulSoup(web_page.read())

## "p" tags hold statements
statements = soup.find_all("p")

## looping over "p" tages
participants = ["LEHRER:", "OBAMA:", "ROMNEY:"]
debate = [""] ## starting with something as first element, delete later
for turn in statements:
	## work with text of each p tag
	text = turn.get_text().encode("UTF-8")

	## if all upper case, then skip turn bc it is annotation or audience
	if text.isupper():
		continue

	## Three conditions:
	## 1) add new element to debate list if speaker name is first word in "p"
	## tag and name is not in previous debate turn
	## 2) add text to previous debate turn and remove speaker name if speaker was in
	## previous debate turn
	## 3) add text to previous debate turn if no speaker was provided
	for p in participants:
		if p in text.split(" ")[0]:
			if p not in debate[-1]:
				debate.append(text)
			else:
				debate[-1] += text.replace(p, " ")
			break
	else:
		debate[-1] += text

del debate[0]

###############
## Problem 2 ##
###############

## Loading positive and negative words
pos = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/positive.txt").read()
pos = pos.split("\n")

neg = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/negative.txt").read()
neg = neg.split("\n")

## Using stemmers
st_pos = [st.stem(i) for i in pos]
pt_pos = [pt.stem(i) for i in pos]
sb_pos = [sb.stem(i) for i in pos]

st_neg = [st.stem(i) for i in neg]
pt_neg = [pt.stem(i) for i in neg]
sb_neg = [sb.stem(i) for i in neg]

## loading stopwords (provided link didn't work)
## making list into same format as my other text so I can process it, too
stopwords = " ".join(get_stop_words('en'))

## helper function to count words in turn from provided list
def count_words(turn, words):
	return len([x for x in turn if x in words])

## helper function to count words in turn NOT in provided list
def count_nonwords(turn, words):
	return len([x for x in turn if x not in words])

## processing and writing data from each turn to csv
with open("debate_data.csv", 'ab') as f:
	## setting up csv
	w = csv.DictWriter(f, fieldnames = ("id", "speaker", \
		"nonstopwords", "pos", "neg", "st_pos", "st_neg", \
		"pt_pos", "pt_neg", "sb_pos", "sb_neg"))
	w.writeheader()

	## processing stop words, as well
	stopwords = re.sub(r"\W", " ", stopwords) ## remove punctuation
	stopwords = stopwords.lower() ## lower case
	stopwords = word_tokenize(stopwords) ## tokenize

	for i, turn in enumerate(debate):
		turn = re.sub(r"\W", " ", turn) ## remove punctuation
		turn = turn.lower() ## lower case
		turn = word_tokenize(turn) ## tokenize

		## call helper functions to get data entry
		row = ({"id" : i,
			"speaker" : turn[0],
			"nonstopwords" : count_nonwords(turn, stopwords),
			"pos" : count_words(turn, pos),
			"neg" : count_words(turn, neg),
			"st_pos" : count_words(turn, st_pos),
			"st_neg" : count_words(turn, st_neg),
			"pt_pos" : count_words(turn, pt_pos),
			"pt_neg" : count_words(turn, pt_neg),
			"sb_pos" : count_words(turn, sb_pos),
			"sb_neg" : count_words(turn, sb_neg)
			}) 
		w.writerow(row)










