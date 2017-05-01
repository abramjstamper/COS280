#!/usr/bin/env python3
from random import randint
import sys

def generateOnePerm():
  perm = []
  for i in range(4):
    randomInt = randint(0,2)
    if randomInt == 0:
      perm.append("F")
    elif randomInt == 1:
      perm.append("T")
    elif randomInt == 2:
      perm.append("#")
  return perm

def generateManyPerms(previous, n):
  perms = []
  for i in range(n):
    perms.append(generateOnePerm())
  return perms+previous

def scoreSolution(perm):
	students = [True, True, True, False, True, False]
	score = 0
	for i in range(6):
		for j in range(len(perm)):
			if(perm[j] == "T" and students[i]):
				score += 1	
	print(perm, end="")
	print(" - " + str(score))
	
	return score
	
def evaluateSolutions(perms):
	scores = []
	for i in range(len(perms)):
		scores.append(scoreSolution(perms[i]))
	return scores

def gatherHighScores(perms, scores, numberToMutate):
	returnVal = []
	for i in range(numberToMutate):
		maxi = max(scores)
		index = scores.index(maxi)
		returnVal.append(perms[index])
	return returnVal

def mutate(previousPerms):
  for i in range(len(previousPerms)):
    position = randint(0,3)
    change = randint(0,2)
    if change == 0:
      previousPerms[i][position] = "F"
    elif change == 1:
      previousPerms[i][position] = "T"
    elif change == 2:
      previousPerms[i][position] = "#"	
  return previousPerms
  
def checkIfDone(perms, scores):
  for i in range(len(scores)):
    if(scores[i] > 20):
    	return True
  return False
  
def printStats():
	print("Statistics Display")

def main():
  print("Genetic Program")
  if(len(sys.argv) == 0):
    print("No arguement for numer of iterations")
    sys.exit(1)
    
  itterations = int(sys.argv[1])
  previous = generateManyPerms([], int(itterations/4))
  
  for i in range(itterations):
    perms = generateManyPerms(previous, itterations)
    print("Generation: " + str(i))
    scores = evaluateSolutions(perms)
    previous = gatherHighScores(perms, scores, int(itterations/4))
    if(checkIfDone(previous, scores)):
    	print("FOUND!")
    	exit(0)
    previous = mutate(previous)
  print("NOT FOUND!")


main()