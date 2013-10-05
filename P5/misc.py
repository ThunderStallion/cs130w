#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if (l == None): 
      return None
    else:
      return min((abs(v - i), i) for i in l)[1]


def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    dictionary = {}
    for key, value in zip(keys,values):
      dictionary[key] = value
    return dictionary
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    word_count = {}
    inFile = open(fn,'r')
    delimiter = re.compile('[^A-Za-z0-9_]')
    for line in inFile:
      words = delimiter.split(line.lower())
      for word in words:
        if word in word_count: 
          word_count[word] = word_count[word] + 1
        else: 
          word_count[word] = 1
    del word_count['']
    return word_count

