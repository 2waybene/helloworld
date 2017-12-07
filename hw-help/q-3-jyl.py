import random

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
         print(aStudent)
         encryt(myKey1,myKey2,aStudent,myStudent2SecretFile)

myStudent2SecretFile.write(str(myKey1))
myStudent2SecretFile.write(str(myKey2))

