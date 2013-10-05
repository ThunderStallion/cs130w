from misc import *
import crypt

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    inFile = open(filename,'r')
    words_loaded = []
    word = ""
    for line in inFile:
      word = re.match(regexp,line)
      if(word == None): pass
      else: words_loaded.append((word.group(0)))
    return words_loaded

def transform_reverse(str):
    """Return a list with the original string and the reversal of the original 
       string."""
    reversed_list = [str]
    reverseString = str[::-1]
    reversed_list[len(reversed_list):] = [reverseString]
    return reversed_list

def transform_capitalize(str):
    """Return a list of all the possible ways to capitalize the input string.
       """
    temp = str.lower()
    words = [temp]
    i = 0
    for element in words:
      for character in element:
        i = 0
        while(i < len(element)):
          s = element
          s = s[:i] + s[i].upper() + s[i+1:]
          if (s not in words): words.append(s)
          i += 1
    return words

def transform_digits(str):
    """Return a list of all possible ways to replace letters with similar 
       looking digits according to the following mappings. This should be done
       without regard to the capitalization of the input string, however when a
       character is not replaced with a digit, it's capitalization should be 
       preserved."""
    temp = str
    words = [temp]
    i = 0
    for element in words:
      for character in element:
        while(i < len(element)):
          s = element
          if (s[i] == 'o' or s[i] == 'O'):
            s = s[:i] + '0' + s[i+1:]
          elif (s[i] == 'z' or s[i] == 'Z'):
            s = s[:i] + '2' + s[i+1:]
          elif (s[i] == 'a' or s[i] == 'A'):
            s = s[:i] + '4' + s[i+1:]
          elif (s[i] == 'b'):
            s = s[:i] + '6' + s[i+1:]
          elif (s[i] == 'B'):
            s = s[:i] + '8' + s[i+1:]
            if (s not in words): words.append(s)
            s = s[:i] + '6' + s[i+1:]
          elif (s[i] == 'i' or s[i] == 'I'):
            s = s[:i] + '1' + s[i+1:]
          elif (s[i] == 'l' or s[i] == 'L'):
            s = s[:i] + '1' + s[i+1:]
          elif (s[i] == 'e' or s[i] == 'E'):
            s = s[:i] + '3' + s[i+1:]
          elif (s[i] == 's' or s[i] == 'S'):
            s = s[:i] + '5' + s[i+1:]
          elif (s[i] == 't' or s[i] == 'T'):
            s = s[:i] + '7' + s[i+1:]
          elif (s[i] == 'g' or s[i] == 'G'):
            s = s[:i] + '9' + s[i+1:]
          elif (s[i] == 'q' or s[i] == 'Q'):
            s = s[:i] + '9' + s[i+1:]
          else: pass
          if (s not in words): words.append(s)
          i = i + 1
        i = 0
    return words

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    return crypt.crypt(plain,enc[:2]) == enc

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    inFile = open(filename,'r');
    passwords = []
    for s in inFile:
      list1 = ['account','password','UID','GID','GECOS','directory','shell']
      list2 = re.split('[:]',s)
      dictionary = make_dict(list1,list2)
      passwords.append(dictionary)
    return passwords

def crack_pass_file(fn_pass,words,out):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    passwords = load_passwd(fn_pass)
    output = open(out,'w')
    account_cracked = []
    for password in passwords:
      if ((password['account'] != '\n') and (password['password'] != None)):
        account = password['account']
        pw = password['password']
        successful_crack = ''
        success_or_fail = False
        possibles = open(words,'r')
        for possible in possibles:
          if(success_or_fail == True): break
          possible = possible.rstrip()
          possible = possible.lower()
          if (len(possible) > 8 or len(possible) < 6): pass
          else:        
            reverseList = transform_reverse(possible)
            for reverse in reverseList:
              success_or_fail = check_pass(reverse,pw)
              if(success_or_fail == True):
                successful_crack = reverse
                account_cracked.append(account)
                output.write(account + '=' + successful_crack + '\n')
                output.flush()
                break
              else: pass
    passwords = load_passwd(fn_pass)
    for password in passwords:
      if ((password['account'] != '\n') and (password['password'] != None)):
        if (password['account'] in account_cracked): pass
        else:
          account = password['account']
          pw = password['password']
          successful_crack = ''
          success_or_fail = False
          possibles = open(words,'r')
          j = 479625
          for possible in possibles:
            print(j)
            j = j - 1
            if(success_or_fail == True): break
            possible = possible.rstrip()
            possible = possible.lower()
            if (len(possible) > 8 or len(possible) < 6): pass
            else:
              reverseList = transform_reverse(possible)
              for reverse in reverseList:
                if (success_or_fail == True): break
                digitList = transform_digits(reverse)
                for digit in digitList:
                  success_or_fail = check_pass(digit,pw)
                  if (success_or_fail == True):
                    successful_cracked = digit
                    account_cracked.append(account)
                    print(account + '=' + successful_crack + '\n')
                    output.write(account + '=' + successful_crack + '\n')
                    output.flush()
                    break
                  else: pass
