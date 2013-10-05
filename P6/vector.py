from misc import Failure

class Vector(object):

    def __init__(self,arg):
      """Default constructor for the Vector instance. If arg is an negative int
         or long, a ValueError exception will be thrown. Otherwise, we 
         construct a Vector instance of length arg with each element 
         initialized to 0.0. If arg is an instance of list, then initialize 
         vector with the length and values in the list. If arg is not any of 
         the type I just mentioned, a TypeError exception will be thrown."""
      if (isinstance(arg,int) or isinstance(arg,long)):
        if (arg < 0): raise ValueError("Vector length can't be negative")
        elif (arg == 0): self.vector = []
        else: self.vector = [0.0]*arg
      elif (isinstance(arg,list)): self.vector = arg
      elif (isinstance(arg,tuple)): self.vector = arg
      else: raise TypeError("Wrong Type")

    def __repr__(self):
      """Return a string of python code which could be used to initialize the 
         Vector."""
      return "Vector(" + str(self.vector) + ")"

    def __len__(self):
      """Return the length of the vector."""
      return len(self.vector)

    def __iter__(self): 
      """Return an object that can iterator over the elements of the Vector."""
      for elt in self.vector: yield elt

    def __add__(self,other):
      """Implement the + and += operators."""
      def helper(x,y): return x + y
      if (isinstance(other,Vector)): 
        return Vector(map(helper,self.vector,other.vector))
      else: return Vector(map(helper,self.vector,other))

    def __radd__(self,other):
      """See __add___(self,other)."""
      return self + other

    def __iadd__(self,other):
      """See ___add___(self,other)."""
      return self + other

    def dot(self,other):
      """Return the dot product of two vectors."""
      def helper(x,y): return x * y
      if (isinstance(other,Vector)): 
        return sum(map(helper,self.vector,other.vector))
      else: return sum(map(helper,self.vector,other))

    def __getitem__(self,index):
      """Return the data at the specified index of the Vector instance."""
      if (index < 0): 
        while(index < 0): index = index + len(self.vector)
        return self.vector[index]
      else: return self.vector[index]

    def __setitem__(self,index,data):
      """Mutate the value at the specified index of the Vector with data."""
      if (index < 0): 
        while(index < 0): index = index + len(self.vector)
        self.vector[index] = data
        return self
      else:
        self.vector[index] = data
        return self

    def __getslice__(self,i,j):
      return self.vector[i:j]

    def __setslice__(self,i,j,sequence):
      self.vector[i:j] = sequence
      return self
    
    def __lt__(self,other):
      """Return True if self < other."""
      list1 = self.vector
      if (isinstance(other,list)): list2 = other
      else: list2 = other.vector
      while((list1 != []) and (list2 != [])):
        if (max(list1) < max(list2)): return True
        elif (max(list1) > max(list2)): return False
        else:
          list1.remove(max(list1))
          list2.remove(max(list2))
      if (len(list1) < len(list2)): return True
      else: return False

    def __le__(self,other):
      """Return True if self <= other."""
      list1 = self.vector
      if (isinstance(other,list)): list2 = other
      else: list2 = other.vector
      while((list1 != []) and (list2 != [])):
        if (max(list1) < max(list2)): return True
        elif (max(list1) > max(list2)): return False
        else:
          list1.remove(max(list1))
          list2.remove(max(list2))
      if (len(list1) <= len(list2)): return True
      else: return False      

    def __eq__(self,other):
      """Return True if self == other."""
      list1 = self.vector
      if (not isinstance(other,Vector)): return False
      else: list2 = other.vector
      if (len(list1) != len(list2)): return False
      else:
        for i in range(0,len(list1)): 
          if (list1[i] != list2[i]): return False
        return True
                                
    def __ne__(self,other):
      """Return True if self != other."""
      list1 = self.vector
      if (not isinstance(other,Vector)): return True
      else: list2 = other.vector
      if (len(list1) != len(list2)): return True
      else:
        for i in range(0,len(list1)): 
          if (list1[i] != list2[i]): return True
        return False

    def __gt__(self,other):
      """Return True if self > other."""
      list1 = self.vector
      if (isinstance(other,list)): list2 = other
      else: list2 = other.vector
      while((list1 != []) and (list2 != [])):
        if (max(list1) > max(list2)): return True
        elif (max(list1) < max(list2)): return False
        else:
          list1.remove(max(list1))
          list2.remove(max(list2))
      if (len(list1) > len(list2)): return True
      else: return False

    def __ge__(self,other):
      """Return True if self >= other."""
      list1 = self.vector
      if (isinstance(other,list)): list2 = other
      else: list2 = other.vector
      while((list1 != []) and (list2 != [])):
        if (max(list1) > max(list2)): return True
        elif (max(list1) < max(list2)): return False
        else:
          list1.remove(max(list1))
          list2.remove(max(list2))
      if (len(list1) >= len(list2)): return True
      else: return False


