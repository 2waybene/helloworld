#Question 1


m = int(input("Please tell me the maximum number of beans a player can take.\n"))
n = int(input("Please tell me the total number of beans at the beginning.\n"))

def myFirstMove(m,n):
    myFirstMove = (n-m-2)%(m+1)
    return myFirstMove
    if n == (m + 2):
        return (0)
print(myFirstMove(m,n))

#Question 2

myStudent2File = open("students.txt","w", encoding="utf-8")
myStudent2File.write("Kimberely Matthias;1;F\nDarron Mouser;2;F\nGarret Emrich;3;M\nLi Townsel;4;F\nJaniece Devera;5;F\nWyatt Kulesza;6;M\nSana Pickford;7;F\nPearlene Jeanlouis;8;M\nSmith Jason;9;M\nEmch Mary;10;F\nBand Kohn;11;M\nMartin Haiwa;12;F")
myStudent2File.close()


#Question 3

import random
myStudent2File = open("students.txt", "r", encoding="utf-8")
myStudent2SecretFile = open("students_secret.txt", "w", encoding="utf-8")
allStudents = myStudent2File.readlines()
myKey1 = random.randint(5,10)
myKey2 = random.randint(5,10)
aLineStr = ''
for aStudent in allStudents:
    if len(aStudent)%2 == 0:
        for aChar in aStudent:
            if aChar == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChar) + myKey2)
    else:
        for aChar in aStudent:
            if aChar == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChar) + myKey1)
        myStudent2SecretFile.write(aLineStr+"\n")
    aLineStr = ''

myStudent2SecretFile.write(str(myKey1))
myStudent2SecretFile.write(str(myKey2))
myStudent2SecretFile.close()
myStudent2File.close()

#Question 4

myStudent2SecretFile = open("students_secret.txt", "r", encoding="utf-8")
myStudent2ClearFile = open("students_clear.txt", "w", encoding="utf-8")

allSecret = myStudent2SecretFile.readlines()
myKey = int(allSecret[-1])
aLineStr = ''
for aSecret in allSecret:
    if len(aSecret)>2:
        for aChr in aSecret:
            if aChr == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChr) - myKey)
            myStudent2ClearFile.write(aLineStr+"\n")
    aLineStr=''

myStudent2ClearFile.close()
myStudent2SecretFile.close()

#Question 5

myStudent2File = open("students.txt", "r", encoding="utf-8")
myfemaleStudent2File = open("femalestudents.txt","w", encoding="utf-8")
mymaleStudent2File = open("malestudents.txt","w", encoding="utf-8")

allStudents = myStudent2File.readlines()

aNewList = []
for i in allStudents:
    aNewList.append(i.strip())

finalList = [i.split(';') for i in aNewList]

femaleStudents=[]
maleStudents=[]

for aList in finalList:
    if 'F' in aList:
        femaleStudents.append(aList)       
        
    if 'M' in aList:
        maleStudents.append(aList)
        
        
import random
luckyNumber1 = random.randint(1,len(femaleStudents))
luckyNumber2 = random.randint(1,len(maleStudents))
print("The lucky number for female students is %i." % luckyNumber1)
print("The lucky female student is %s." % ';'.join (femaleStudents[luckyNumber1-1]))

print("The lucky number for male students is %i." % luckyNumber2)
print("The lucky male student is %s." % ';'.join(maleStudents[luckyNumber2-1]))


myfemaleStudent2File.write(str(femaleStudents))
mymaleStudent2File.write(str(maleStudents))
myStudent2File.close()
myfemaleStudent2File.close()
mymaleStudent2File.close()


#Question 6

myfemaleStudent2File = open("femalestudents.txt", "r", encoding="utf-8")
femaleStudents = myStudent2File.readlines()
numStudents = len(femaleStudents)
luckyCountList = []
for i in range(numStudents):
    luckyCountList.append(0)
allLucky = False

import random

while(not(allLucky)):
    luckyNumber = random.randint(0,numStudents-1)
    luckyCountList[luckyNumber] = luckyCountList[luckyNumber] + 1
    allLucky = True
    for aNum in luckyCountList:
        if aNum == 0:
            allLucky = False
            break

luckySorted = sorted(luckyCountList,reverse = True)
luckyTimes = luckySorted[0]
for i in range(numStudents):
    if luckyCountList[i] == luckyTimes:
        print("The luckiest femalestudent is %s" % femaleStudents[i].split(";")[0])
        
myfemaleStudent2File.close()




