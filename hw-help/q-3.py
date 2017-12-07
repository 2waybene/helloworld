import random

#myStudent2File = open("students.txt", "r", encoding="utf-8")
#myStudent2SecretFile = open("students_secret.txt", "w", encoding="utf-8")

myStudent2File = open("test.txt", "r", encoding="utf-8")
myStudent2SecretFile = open("test_secret.txt", "w", encoding="utf-8")

allStudents = myStudent2File.readlines()

print (allStudents)

myKey1 = random.randint(5,9)
myKey2 = random.randint(5,9)
#aLineStr = ''

'''
for aStudent in allStudents:
    if len(aStudent)%2 == 0:
        for aChar in aStudent:
            if aChar == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChar) + myKey2)
        print(aLineStr)
        myStudent2SecretFile.write(aLineStr+"\n")
    else:
        for aChar in aStudent:
            if aChar == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChar) + myKey1)
        myStudent2SecretFile.write(aLineStr+"\n")
        print(aLineStr)
    aLineStr = ''
    
    
    for aStudent in allStudents:
    if len(aStudent)>1:
        for aChr in aStudent:
            if aChr == "\n":
                continue
            aLineStr = aLineStr + chr(ord(aChr) + key)
        fStudentSecret.write(aLineStr+"\n")
    aLineStr=''

    
'''

def encrypt (key1,key2,allStudents,outStream):
    aLineStr = ''
    for aStudent in allStudents:
        if len(aStudent)>1:
            index = 0
            for aChr in aStudent:
               if aChr == "\n":
                    continue
               else:
                   if (index % 2) ==0 :
                        aLineStr = aLineStr + chr(ord(aChar) + key1)
                   else:
                        aLineStr = aLineStr + chr(ord(aChar) + key2)
                    index = index +1
            outStream.write(aLineStr+"\n")
        aLineStr=''

encrypt (myKey1, myKey2, allStudents,myStudent2SecretFile)

myStudent2SecretFile.write(str(myKey1))
myStudent2SecretFile.write(str(myKey2))
myStudent2SecretFile.close()
myStudent2File.close()

