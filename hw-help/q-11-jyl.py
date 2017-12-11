
#Question 11
#Fucntion for q11

def deEncrytStory (keyList, word):
    aLineStr = ''
    keyListToUse = keyList[:]
    keyTemp = keyListToUse.pop(0)
    for aChr in word:
        if (len(keyListToUse) > 0):
            aLineStr = aLineStr + chr(ord(aChr) - int(keyTemp))
            keyTemp = keyListToUse.pop(0)
        elif (len(keyListToUse) == 0):
            aLineStr = aLineStr + chr(ord(aChr) - int(keyTemp))
            keyListToUse = keyList[:]
            keyTemp = keyListToUse.pop(0)
        else:
            print ("Something is wrong here\n")
    return(aLineStr)


mySecretStoryFile = open("story_secret.txt", "r", encoding="utf-8")
myClearStoryFile  = open("story_clear.txt",  "w", encoding="utf-8")
allSecretStory = mySecretStoryFile.readlines()
allKeys = allSecretStory[-1]
allKeys = allKeys.strip("\n")
keyList = allKeys.split(" ")
keyList = keyList[:-1]

story = ""
for x in range (0, (len(allSecretStory) -1)):
    aDecodedLine = deEncrytStory(keyList, allSecretStory[x])
    story = story + aDecodedLine
    myClearStoryFile.write(aDecodedLine+"\n")

print (story)

