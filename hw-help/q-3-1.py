# Question 3 - encrpyt the students.txt file using a random number key
import random
#fStudent = open("students.txt", "r", encoding="utf-8")
#fStudentSecret = open("students_secret.txt", "w", encoding="utf-8")

#fStudent = open("students.txt", "r", encoding="utf-8")
fStudent = open("test.txt", "r", encoding="utf-8")
fStudentSecret = open("test_secret.txt", "w", encoding="utf-8")

allStudents = fStudent.readlines()
key1 = random.randint(5,10)
key2 = random.randint(5,10)
aLineStr = ''
for aStudent in allStudents:
    if len(aStudent)>1:
        counter = 0
        for aChr in aStudent:
            counter = couner +1
            if aChr == "\n":
                continue
            print(aChr)
            if (counter % 2 )== 0:
                aLineStr = aLineStr + chr(ord(aChr) + key2)
            else:
                aLineStr = aLineStr + chr(ord(aChr) + key1)
        fStudentSecret.write(aLineStr+"\n")
    aLineStr=''

fStudentSecret.write(str(key))
fStudentSecret.close()
fStudent.close()
