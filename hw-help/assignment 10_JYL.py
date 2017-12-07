
#Question 1


#Fucntion for q1
def myFirstMove(m,n):
    if n==m+2:
        return(0)
    else:
        myFirstMove = (n-m-2)%(m+1)
        return myFirstMove


m = int(input("Please tell me the maximum number of beans a player can take.\n"))
n = int(input("Please tell me the total number of beans at the beginning.\n"))

if n <= m + 1:
    print ("You must enter n is greater than m + 1")
    exit(1)
else:
    maxnum = myFirstMove(m,n)
    if (maxnum > 0):
        print ("To ensure winning the game, the maximum number to choose is " + str(maxnum))
    else:
        print ("Sorry, you are loosing the game: "  + str(maxnum))

print ("Finished question 1\n")
#Question 2

myStudent2File = open("students.txt","w", encoding="utf-8")
myStudent2File.write("Kimberely Matthias;1;F\nDarron Mouser;2;F\nGarret Emrich;3;M\nLi Townsel;4;F\nJaniece Devera;5;F\nWyatt Kulesza;6;M\nSana Pickford;7;F\nPearlene Jeanlouis;8;M\nSmith Jason;9;M\nEmch Mary;10;F\nBand Kohn;11;M\nMartin Haiwa;12;F")
myStudent2File.close()

print ("Finished question 2\n")
#Question 3
import random
#Fucntion for q3
def encryt (key1, key2, aStudent, outstream):
    counter = 0
    aLineStr=''
    for aChr in aStudent:
        counter = counter + 1
        if aChr == "\n":
            continue
        if (counter % 2 )== 0:
            aLineStr = aLineStr + chr(ord(aChr) + key2)
        else:
            aLineStr = aLineStr + chr(ord(aChr) + key1)
    outstream.write(aLineStr + "\n")

myStudent2File = open("students.txt", "r", encoding="utf-8")
myStudent2SecretFile = open("students_secret.txt", "w", encoding="utf-8")

allStudents = myStudent2File.readlines()

myKey1 = random.randint(5,10)
myKey2 = random.randint(5,10)
for aStudent in allStudents:
     if len(aStudent)>1:
        # print(aStudent)
         encryt(myKey1,myKey2,aStudent,myStudent2SecretFile)

myStudent2SecretFile.write(str(myKey1))
myStudent2SecretFile.write(str(myKey2))

print ("Finished question 3\n")
#Question 4

#Fucntion for q4
def deEncryt (key1, key2, aStudent, outstream):
    counter = 0
    aLineStr=''
    for aChr in aStudent:
        counter = counter + 1
        if aChr == "\n":
            continue
        if (counter % 2 )== 0:
            aLineStr = aLineStr + chr(ord(aChr) - key2)
        else:
            aLineStr = aLineStr + chr(ord(aChr) - key1)
    outstream.write(aLineStr + "\n")


myStudent2SecretFile = open("students_secret.txt", "r", encoding="utf-8")
myStudent2ClearFile = open("students_clear.txt", "w", encoding="utf-8")
allSecret = myStudent2SecretFile.readlines()
myKeys = allSecret[-1]

##  Get my keys back
if len(myKeys) == 3:
   # print (myKeys)
    if myKeys[0] == "1":
        myKey1 = "10"
        myKey2 = myKeys[2]
    else:
        myKey1 = myKeys[0]
        myKey2 = "10"
else:
    myKey1 = myKeys[0]
    myKey2 = myKeys[1]

myKey1 = int(myKey1)
myKey2 = int(myKey2)

##  Now, decoding..
allSecret = allSecret[:-1]
for aSecret in allSecret:
     if len(aSecret)>1:
       #  print(aSecret)
         deEncryt(myKey1,myKey2,aSecret,myStudent2ClearFile)


print ("Finished question 4\n")

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
print ("\n")
print("The lucky number for male students is %i." % luckyNumber2)
print("The lucky male student is %s." % ';'.join(maleStudents[luckyNumber2-1]))


myfemaleStudent2File.write(str(femaleStudents))
mymaleStudent2File.write(str(maleStudents))
myStudent2File.close()
myfemaleStudent2File.close()
mymaleStudent2File.close()

print ("Finished question 5\n")
#Question 6

import random
import operator
#Fucntion for q6
def populateLuckyDict (luckyM, luckyF, selected):
    selected = selected.strip("\n")
    inStudent = selected.split(';')
    gender = inStudent[-1]
    name = inStudent[0]
    if gender == "F":
        #luckyF,
        if name in luckyF.keys():
            count = luckyF[name] + 1
            luckyF[name] = count
        else:
            luckyF[name] = 1
    elif gender == "M":
        #luckyM,
        if name in luckyM.keys():
            count = luckyM[name] + 1
            luckyM[name] = count
        else:
            luckyM[name] = 1
    else:
        print ("Something wrong")
        print ("Gender\n")
        exit(1)


myStudent2File = open("students.txt", "r", encoding="utf-8")
recordFile = open("luck_numbers.txt", "w", encoding="utf-8")

allStudents = myStudent2File.readlines()
aNewList = []

aLuckyMaleDict = {}
aLuckyFemaleDict = {}



while (set(allStudents) != set(aNewList)):
    luckyNumber = random.randint(1,len(allStudents))
    recordFile.write(str(luckyNumber)+"\n")
    luckStudentTemp = allStudents[luckyNumber-1]
    if luckStudentTemp not in aNewList:
        aNewList.append(luckStudentTemp)
    populateLuckyDict (aLuckyMaleDict,aLuckyFemaleDict,luckStudentTemp)



print ("Male luckiest student count:")
sorted_M = sorted(aLuckyMaleDict.items(), key=operator.itemgetter(1))
print(sorted_M[-1][0])


print ("Female luckiest student count:")
sorted_F = sorted(aLuckyFemaleDict.items(), key=operator.itemgetter(1))
print(sorted_F[-1][0])

print ("Finished question 6, all doen!\n")

