
m = int(input("Please tell me the maximum number of beans a player can take.\n"))
n = int(input("Please tell me the total number of beans at the beginning.\n"))



def myFirstMove(m,n):
    if n==m+2:
        return(0)
    else:
        myFirstMove = (n-m-2)%(m+1)
        return myFirstMove


if n <= m + 1:
    print ("You must enter n is greater than m + 1")
    exit(1)
else:
    maxnum = myFirstMove(m,n)
    if (maxnum > 0):
        print ("To ensure winning the game, the maximum number to choose is " + str(maxnum))
    else:
        print ("Sorry, you are loosing the game: "  + str(maxnum))
