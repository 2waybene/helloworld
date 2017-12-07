# Name: Jun Liang
import codecs
print("My name is Jun Liang.")

# Question 2 - write file
fileStudent = open("students.txt", "w", encoding="utf-8")
fileStudent.write("Kimberely Matthias;1\nDarron Mouser;2\nGarret Emrich;3\nLi Townsel;4\nJaniece Devera;5\nWyatt Kulesza;6\nSana Pickford;7\nPearlene Jeanlouis;8\n")
fileStudent.close()

# Question 3 - encrpyt the students.txt file using a random number key
import random
fStudent = open("students.txt", "r", encoding="utf-8")
fStudentSecret = open("students_secret.txt", "w", encoding="utf-8")
allStudents = fStudent.readlines()
key = random.randint(5,10)
aLineStr = ''
for aStudent in allStudents:
    if len(aStudent)>1:
        for aChr in aStudent:
            if aChr == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChr) + key)
        fStudentSecret.write(aLineStr+"\n")
    aLineStr=''

fStudentSecret.write(str(key))
fStudentSecret.close()
fStudent.close()

# Question 4 - decrypt the students_secret.txt
fStudentSecret = open("students_secret.txt", "r", encoding="utf-8")
fStudentClear = open("students_clear.txt", "w", encoding="utf-8")

allScret = fStudentSecret.readlines()
key = int(allScret[-1])
aLineStr = ''
for aScrete in allScret:
    if len(aScrete)>2:
        for aChr in aScrete:
            if aChr == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChr) - key)
        fStudentClear.write(aLineStr+"\n")
    aLineStr=''

fStudentClear.close()
fStudentSecret.close()

# Question 5 - Pick a lucky student from students.txt
fStudent = open("students.txt", "r", encoding="utf-8")
allStudents = fStudent.readlines()
numStudents = len(allStudents)
print("The total number of students is %i" % numStudents)

luckyNumber = random.randint(0,numStudents-1)
print("The lucky number is %i" % luckyNumber)
print("The lucky student is %s" % allStudents[luckyNumber].split(";")[0])

fStudent.close()

# Question 6 - Bonus - Find the luckiest student
fStudent = open("students.txt", "r", encoding="utf-8")
allStudents = fStudent.readlines()
numStudents = len(allStudents)
luckyCountList = []
for i in range(numStudents):
    luckyCountList.append(0)
allLucky = False
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
        print("The luckiest student is %s" % allStudents[i].split(";")[0])
        
fStudent.close()
