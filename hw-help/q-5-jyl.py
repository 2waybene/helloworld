
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

