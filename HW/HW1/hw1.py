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
## after applying the appropriate stemmer to the turn
def count_words(turn, words, stemmer = "none"):
	if stemmer == "st": turn = [st.stem(i) for i in turn]
	elif stemmer == "pt": turn = [pt.stem(i) for i in turn]
	elif stemmer == "sb": turn = [sb.stem(i) for i in turn]
	elif stemmer == "none": pass
	else: raise Exception
	return len([x for x in turn if x in words])

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

	for index, turn in enumerate(debate):
		turn = re.sub(r"\W", " ", turn) ## remove punctuation
		turn = turn.lower() ## lower case
		turn = word_tokenize(turn) ## tokenize
		turn = [i for i in turn if i not in stopwords] ## remove stopwords

		## call helper functions to get data entry
		row = ({"id" : index,
			"speaker" : turn[0],
			"nonstopwords" : len(turn)-1, ## minus the speaker
			"pos" : count_words(turn, pos, "none"),
			"neg" : count_words(turn, neg, "none"),
			"st_pos" : count_words(turn, st_pos, "st"),
			"st_neg" : count_words(turn, st_neg, "st"),
			"pt_pos" : count_words(turn, pt_pos, "pt"),
			"pt_neg" : count_words(turn, pt_neg, "pt"),
			"sb_pos" : count_words(turn, sb_pos, "sb"),
			"sb_neg" : count_words(turn, sb_neg, "sb")
			}) 
		w.writerow(row)










