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



inStudent = "Kimberely Matthias;1;F"
items = inStudent.split(';')
name = ' '.join(items[0:1])
print(name)

luckyF = {}
luckyF[name] = 1

for k,v in luckyF.items():
    print (k, v)


if name in luckyF.keys():
    count = luckyF[name] + 1
    print (count)
    luckyF[name] = count

for k,v in luckyF.items():
    print (k, v)

'''  



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

'''

