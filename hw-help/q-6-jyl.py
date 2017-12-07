
import random
import operator

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


