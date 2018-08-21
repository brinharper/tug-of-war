# tug-of-war

# Experiment 3 README:
 
## What has been done in experiment 3:

* R code has the stimuli from our webppl code

* JavaScript:

 see config.js to see how we can modify the front end to accomodate different questions and comments

* json:
 the stimuli are encoded here
```
{
		  	"id": 0, // the id of the game
			"games": [ // the actual results of the games we see on the screen
				{
					"team1": [1], 
					"team2": [2],
					"winner": 1
				}
			],
			"comments": [], // any comments we'd like the commentator to say in addition to the below ones
			"questions": [1], // which question we are asking. go to the bottom of the json to see them
			"wonngames": [{"player": 1, "nWon": 2, "total": 2}, {"player": 2, "nWon": 1, "total": 2}], // which player has previously one N amount of games before this match
			"strongest": [{"player": 1, "group": [2,3]}], // who is the strongest
			"isstrong": [1], // who is 'strong'
			"isweak": [2], // who is 'weak'
			"closegames": [1], // which games were close 
			"whoiswhat": [{"player": 1, "is": "strong"}, {"player": 2, "is": "weak"}],// new structure for whoiswhat; TODO: refactor the rest of the games to use this format to make it scalable
			"subjects": [{"player": 2, "match": 1}] // which player we are interested in ... subject of the question.
}
```

## To do for this project:

1. Generate a template for easily modeling games.

The template should be a list with the following columns:

row index | game index  (G1B1, G1'B1...)| game info (match list) | background info | question | mean | std dev 

1. Use this template to generate 45 games

G stands for game
B stands for stimuli

A game will have an outcome G1, and the reverse G1'
A game will have a set of stimuli B1, and a complementary set of stimuli B1'

The 3 games that should be generated from this are:

G1B1, G1B1', G1'B1

The idea is to leave one variable, G or B unchanged and to toggle the opposite variable

G1:
	A > B
	B > C 
	B > C

B1:
	strong: [B]
	close: [1]

G1':
	A > B
	B < C 
	B < C

B1':
	strong: [B]
	close: []


------------
G2:
	A > B
	A > B
	A > B

B2:
	strong: []
	close: [1, 2, 3]

G2':
	A < B
	A > B
	A > B

B2':
	strong: []
	close: []

-------------

G3:
	A > B
	A < C
	A > C

B3:
	strong: []
	close: []
	wonngames: [{player: A, nWon: 5, total: 5}]

G3':
	A < B
	A < C
	A < C

B3':
	strong: []
	close: []
	wonngames: [{player: A, nWon: 1, total: 5}]

-----------
 
* Game 4, 5, 6 = G1B1, G1 B1', G1' B1

* Game 7, 8, 9 = G2B2, G2B2', G2'B2

* Game 10, 11, 12 = G3B3, G3B3', G3'B3


* see n_exp3_stim.json for the above mentioned games

* exp3_stim.json is largely unmodified and copied from exp2

* n_exp3_stim.json contains the experimental games with the new stimuli that has been created for exp3

1. Plot results of the various games

* gg plot code for plotting scheme is found in our R files

* ideally get the table from part 2 into an R dataframe 

notes:
use xtable() 
Look at tug analysis.R to see how to generate the tables based on the dataframes 


