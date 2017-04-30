import random

students = ["Richard", "Alan", "Alison", "Jeff", "Gail", "Simon"]
attributes = ["firstLastYear", "male", "worksHard", "drinksALot"]

studentDict = {"Richard": [True, True, False, True, True],
               "Alan": [True, True, True, False, True],
               "Alison": [False, False, True, False, True],
               "Jeff": [False, True, False, True, False],
               "Gail": [True, False, True, True, True],
               "Simon": [False, True, True, True, False]}

firingThreshold = 0.5


def initializeWeights():
    random.seed()

    weights = {"firstLastYear": 0,
               "male": 0,
               "worksHard": 0,
               "drinksALot": 0}
    for attribute in attributes:
        weights[attribute] = random.uniform(-1.00, 1.00)
        weights[attribute] = round(weights[attribute], 2)
    return weights


def sumWeights(firstLastYear, male, worksHard, drinksALot, weights):
    total = 0.0
    if (firstLastYear):
        total += weights["firstLastYear"]
    if (male):
        total += weights["male"]
    if (worksHard):
        total += weights["worksHard"]
    if (drinksALot):
        total += weights["drinksALot"]
    total = round(total, 2)
    return total


def roundWeights(weights):
    for attribute in attributes:
        weights[attribute] = round(weights[attribute], 2)
    return weights


def getStudentAttributes(student):
    studentAttributes = {"firstLastYear": studentDict[student][0],
                         "male": studentDict[student][1],
                         "worksHard": studentDict[student][2],
                         "drinksALot": studentDict[student][3]}
    return studentAttributes


def main():
    weights = initializeWeights()
    numCorrect = 0
    for i in range(0, 40):
        if numCorrect == len(students):
            print("\nFound the correct Solution!")
            break
        print()
        numCorrect = 0
        for student in students:
            studentAttributes = getStudentAttributes(student)
            totalWeight = sumWeights(studentAttributes["firstLastYear"], studentAttributes["male"],
                                     studentAttributes["worksHard"], studentAttributes["drinksALot"], weights)
            firstThisYear = studentDict[student][4]

            # if output is 1 and target output is 0
            if totalWeight >= firingThreshold and (firstThisYear == False):
                for attribute in attributes:
                    if studentAttributes[attribute]:
                        weights[attribute] = weights[attribute] - 0.05

            # if output is 0 and target output is 1
            elif totalWeight < firingThreshold and (firstThisYear == True):
                for attribute in attributes:
                    if studentAttributes[attribute]:
                        weights[attribute] = weights[attribute] + 0.05

            elif totalWeight >= firingThreshold and (firstThisYear == True):
                numCorrect += 1
            elif totalWeight < firingThreshold and (firstThisYear == False):
                numCorrect += 1
        weights = roundWeights(weights)
        print("Generation: " + str(i + 1))
        print("numCorrect: " + str(numCorrect) + "/6")
        print("Weights: " + str(weights["firstLastYear"]) + ", " + str(weights["male"]) + ", " + str(
            weights["worksHard"]) + ", " + str(weights["drinksALot"]))


main()
