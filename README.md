# Marvel-Dataset
Data and code for txtlab Superhero SNA project
This is the dataset used for a project associated with McGill University's .txtLAB. 
Code included here is able to generate the attached dataset (before cleaning, standardizing, and annotating was done by hand.) 

The universe_spiders are intended to be used with the scraPy package and will generate character lists for the networks discussed in the paper.
edge_gen.py can be run on these character lists and will output edge lists compatible with the R scripts for each respective universe. Cleaning of redundancies / minor characters should be done before this step. 

The remaining R scripts generate the results / graphics in the paper (link when that is posted @ me). 
