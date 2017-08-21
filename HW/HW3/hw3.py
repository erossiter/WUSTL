import urllib2
import re
import csv
from numpy import std
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
	with open("stories/doc" + str(i+1) + ".txt", "w") as f:
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
desks = [i["meta"]["dsk"] for i in nyt_data]

## grabbing date
day = [i["meta"]["publication_day_of_month"] for i in nyt_data]

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


## Clustering Methods part of exercise is in R script ##



################################################
## Part 2 : Dictionary Classification Methods ##
################################################

## Loading positive and negative words
## Processing them as I did the text
pos = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/positive.txt").read()
pos = pos.replace("\n", " ")
pos = process_string(pos, True, stopwords)

neg = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/negative.txt").read()
neg = neg.replace("\n", " ")
neg = process_string(neg, True, stopwords)


## Assuming each word in the lists above have equal weight of 1 and words not in the
## lists have weight of zero, calculating positive and negative scores for each doc
pos_scores = []
neg_scores = []
for story in processed_nyt:
	p_count = len([i for i in story if i in pos])
	n_count = len([i for i in story if i in neg])
	pos_scores.append(float(p_count) / len(story))
	neg_scores.append(float(n_count) / len(story))

## diff > 0 means more pos
## diff < 0 means more neg
diff_in_scores = [x - y for x, y in zip(pos_scores, neg_scores)]


## Evaluating scores by desk first
def desk_function(d):
	out = []
	for i, score in enumerate(diff_in_scores):
		if desks[i] == d:
			out.append(score)
	mean = sum(out)/len(out)
	sd = std(out)
	return mean, sd

## desk, mean, sd
scores_by_desk = zip(set(desks), map(desk_function, set(desks)))

''' I took the average difference score for each desk.  All just
barely had a net positive mean score.  First consider the Metropolitan
Desk.  A score of .028 with a standard deviation of .051.  Also consider
the Foreign Desk with a score of .012 and an sd of .05.  These have
both the lowest scores and the highest standard deviations in the desk.
These do seem like the most negative sections of the paper.

Next consider the most positive sections of the paper.  A priori, I would
think the Arts and Cultural Desk would be the highest by far.  It's score
is .06.  This section may give poor reviews every once and a while, generating
negative words, but I feel like the content of the art/culture that the
articles are reviewing should evoke positive words more than any other
section of the paper.  This may be true, but not to the extent that I would
have thought.  Surprisingly, health and fitness is the highest score at .066.
I would have thought this section would draw upon negative words, as we
care about our health and fitness for fear of negative things happening to us
(e.g., cancer, depression, death)! '''

## Evaluating scores before and after election (omitting stories on election day)
## The election was on the 2nd, so above I grabbed what day of the month the
## article ran on in November 2004 to use here.
before = []
after = []
for i, score in enumerate(diff_in_scores):
	if day[i] == "1":
		before.append(score)
	if day[i] == "3":
		after.append(score)

before_mean = sum(before)/len(before)
after_mean = sum(after)/len(after)

''' Both have net positive scores again, although very close to zero.
Before the election, the average difference in score was .043 and the
day after the election, the average difference was .049.  So the 
paper was slightly more positive the day after the election.  We don't
know how much of this increase to attribute to the election, if any at all,
as this paper covers much more than politics, but also sports, health, and
the arts, which all could have uncharactistically positive articles that day,
for example.  It would also help to have a sample of pre- and post-election
articles that extend more than just one day to compare if there was a "mood
shift" in our news.'''



###################################################
## Part 3 : Supervised Learning with Naive Bayes ##
###################################################

## There are 56 business and 76 national stores
## Grabbing just those to use.
business = [processed_nyt[i] for i, d in enumerate(desks) if d == "Business/Financial Desk"]
national = [processed_nyt[i] for i, d in enumerate(desks) if d == "National Desk"]

bus_train = business[::2]
nat_train = national[::2]

n_train = [float(len(bus_train)), float(len(nat_train))]
n = sum(n_train)

p_ck = [n_train[0]/n, n_train[1]/n]

















