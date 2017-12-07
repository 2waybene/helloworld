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
