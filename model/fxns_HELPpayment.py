import math

# Function that tells you the required HELP rate
def HELPrate(income, year):
    if year == 1989:
        if income <= 21999:
            rate = 0
        elif income <= 24999:
            rate = 1
        elif income <= 34999:
            rate = 2
        else:
            rate = 3
    elif year == 1990:
        if income <= 23582:
            rate = 0
        elif income <= 26798:
            rate = 1
        elif income <= 37518:
            rate = 2
        else:
            rate = 3
    elif year == 1991:
        if income <= 25468:
            rate = 0
        elif income <= 28941:
            rate = 2
        elif income <= 40519:
            rate = 3
        else:
            rate = 4
    elif year == 1992:
        if income <= 27097:
            rate = 0
        elif income <= 30793:
            rate = 2
        elif income <= 43112:
            rate = 3
        else:
            rate = 4
    elif year == 1993:
        if income <= 27747:
            rate = 0
        elif income <= 31532:
            rate = 2
        elif income <= 44146:
            rate = 3
        else:
            rate = 4
    elif year == 1994:
        if income <= 26402:
            rate = 0
        elif income <= 30004:
            rate = 3
        elif income <= 42005:
            rate = 4
        else:
            rate = 5
    elif year == 1995:
        if income <= 26852:
            rate = 0
        elif income <= 30516:
            rate = 3
        elif income <= 42722:
            rate = 4
        else:
            rate = 5
    elif year == 1996:
        if income <= 27674:
            rate = 0
        elif income <= 31449:
            rate = 3
        elif income <= 44029:
            rate = 4
        else:
            rate = 5
    elif year == 1997:
        if income <= 28494:
            rate = 0
        elif income <= 30049:
            rate = 3
        elif income <= 32381:
            rate = 3.5
        elif income <= 37563:
            rate = 4
        elif income <= 43335:
            rate = 4.5
        elif income <= 47718:
            rate = 5
        elif income <= 51293:
            rate = 5.5
        else:
            rate = 6
    elif year == 1998:
        if income <= 20700:
            rate = 0
        elif income <= 21830:
            rate = 3
        elif income <= 23524:
            rate = 3.5
        elif income <= 27288:
            rate = 4
        elif income <= 32934:
            rate = 4.5
        elif income <= 34665:
            rate = 5
        elif income <= 37292:
            rate = 5.5
        else:
            rate = 6
    elif year == 1999:
        if income <= 21333:
            rate = 0
        elif income <= 22498:
            rate = 3
        elif income <= 24244:
            rate = 3.5
        elif income <= 28123:
            rate = 4
        elif income <= 33942:
            rate = 4.5
        elif income <= 35726:
            rate = 5
        elif income <= 38402:
            rate = 5.5
        else:
            rate = 6
    elif year == 2000:
        if income <= 21983:
            rate = 0
        elif income <= 23183:
            rate = 3
        elif income <= 24982:
            rate = 3.5
        elif income <= 28980:
            rate = 4
        elif income <= 34976:
            rate = 4.5
        elif income <= 36814:
            rate = 5
        elif income <= 39572:
            rate = 5.5
        else:
            rate = 6
    elif year == 2001:
        if income <= 22345:
            rate = 0
        elif income <= 23565:
            rate = 3
        elif income <= 25393:
            rate = 3.5
        elif income <= 29456:
            rate = 4
        elif income <= 35551:
            rate = 4.5
        elif income <= 37420:
            rate = 5
        elif income <= 40223:
            rate = 5.5
        else:
            rate = 6
    elif year == 2002:
        if income <= 23241:
            rate = 0
        elif income <= 24510:
            rate = 3
        elif income <= 26412:
            rate = 3.5
        elif income <= 30638:
            rate = 4
        elif income <= 36977:
            rate = 4.5
        elif income <= 38921:
            rate = 5
        elif income <= 41837:
            rate = 5.5
        else:
            rate = 6
    elif year == 2003:
        if income <= 24364:
            rate = 0
        elif income <= 25694:
            rate = 3
        elif income <= 27688:
            rate = 3.5
        elif income <= 32118:
            rate = 4
        elif income <= 38763:
            rate = 4.5
        elif income <= 40801:
            rate = 5
        elif income <= 43858:
            rate = 5.5
        else:
            rate = 6
    elif year == 2004:
        if income <= 25347:
            rate = 0
        elif income <= 26731:
            rate = 3
        elif income <= 28805:
            rate = 3.5
        elif income <= 33414:
            rate = 4
        elif income <= 40328:
            rate = 4.5
        elif income <= 42477:
            rate = 5
        elif income <= 45628:
            rate = 5.5
        else:
            rate = 6
    elif year == 2005:
        if income <= 35000:
            rate = 0
        elif income <= 38987:
            rate = 4
        elif income <= 42972:
            rate = 4.5
        elif income <= 45232:
            rate = 5
        elif income <= 48621:
            rate = 5.5
        elif income <= 52657:
            rate = 6
        elif income <= 55429:
            rate = 6.5
        elif income <= 60971:
            rate = 7
        elif income <= 64999:
            rate = 7.5
        else:
            rate = 8
    elif year == 2006:
        if income <= 36184:
            rate = 0
        elif income <= 40306:
            rate = 4
        elif income <= 44427:
            rate = 4.5
        elif income <= 46762:
            rate = 5
        elif income <= 50266:
            rate = 5.5
        elif income <= 54439:
            rate = 6
        elif income <= 57304:
            rate = 6.5
        elif income <= 63062:
            rate = 7
        elif income <= 67199:
            rate = 7.5
        else:
            rate = 8
    elif year == 2007:
        if income <= 38148:
            rate = 0
        elif income <= 42494:
            rate = 4
        elif income <= 46838:
            rate = 4.5
        elif income <= 49300:
            rate = 5
        elif income <= 52994:
            rate = 5.5
        elif income <= 57394:
            rate = 6
        elif income <= 60414:
            rate = 6.5
        elif income <= 66485:
            rate = 7
        elif income <= 70846:
            rate = 7.5
        else:
            rate = 8
    elif year == 2008:
        if income <= 39824:
            rate = 0
        elif income <= 44360:
            rate = 4
        elif income <= 48895:
            rate = 4.5
        elif income <= 51466:
            rate = 5
        elif income <= 55322:
            rate = 5.5
        elif income <= 59915:
            rate = 6
        elif income <= 63068:
            rate = 6.5
        elif income <= 69405:
            rate = 7
        elif income <= 73959:
            rate = 7.5
        else:
            rate = 8
    elif year == 2009:
        if income <= 41598:
            rate = 0
        elif income <= 46333:
            rate = 4
        elif income <= 51070:
            rate = 4.5
        elif income <= 53754:
            rate = 5
        elif income <= 57782:
            rate = 5.5
        elif income <= 62579:
            rate = 6
        elif income <= 65873:
            rate = 6.5
        elif income <= 72492:
            rate = 7
        elif income <= 77247:
            rate = 7.5
        else:
            rate = 8
    elif year == 2010:
        if income <= 43151:
            rate = 0
        elif income <= 48066:
            rate = 4
        elif income <= 52980:
            rate = 4.5
        elif income <= 55764:
            rate = 5
        elif income <= 59943:
            rate = 5.5
        elif income <= 64919:
            rate = 6
        elif income <= 68336:
            rate = 6.5
        elif income <= 75203:
            rate = 7
        elif income <= 80136:
            rate = 7.5
        else:
            rate = 8
    elif year == 2011:
        if income <= 44911:
            rate = 0
        elif income <= 50028:
            rate = 4
        elif income <= 55143:
            rate = 4.5
        elif income <= 58041:
            rate = 5
        elif income <= 62390:
            rate = 5.5
        elif income <= 67750:
            rate = 6
        elif income <= 71126:
            rate = 6.5
        elif income <= 78273:
            rate = 7
        elif income <= 83407:
            rate = 7.5
        else:
            rate = 8
    elif year == 2012:
        if income <= 47195:
            rate = 0
        elif income <= 52572:
            rate = 4
        elif income <= 57947:
            rate = 4.5
        elif income <= 60993:
            rate = 5
        elif income <= 65563:
            rate = 5.5
        elif income <= 71006:
            rate = 6
        elif income <= 74743:
            rate = 6.5
        elif income <= 82253:
            rate = 7
        elif income <= 87649:
            rate = 7.5
        else:
            rate = 8
    elif year == 2013:
        if income <= 49095:
            rate = 0
        elif income <= 54688:
            rate = 4
        elif income <= 60279:
            rate = 4.5
        elif income <= 63448:
            rate = 5
        elif income <= 68202:
            rate = 5.5
        elif income <= 73864:
            rate = 6
        elif income <= 77751:
            rate = 6.5
        elif income <= 85564:
            rate = 7
        elif income <= 91177:
            rate = 7.5
        else:
            rate = 8
    elif year == 2014:
        if income <= 51308:
            rate = 0
        elif income <= 57173:
            rate = 4
        elif income <= 62997:
            rate = 4.5
        elif income <= 66308:
            rate = 5
        elif income <= 71277:
            rate = 5.5
        elif income <= 77194:
            rate = 6
        elif income <= 81256:
            rate = 6.5
        elif income <= 89421:
            rate = 7
        elif income <= 95287:
            rate = 7.5
        else:
            rate = 8
    elif year == 2015:
        if income <= 53345:
            rate = 0
        elif income <= 59421:
            rate = 4
        elif income <= 65497:
            rate = 4.5
        elif income <= 68939:
            rate = 5
        elif income <= 74105:
            rate = 5.5
        elif income <= 80257:
            rate = 6
        elif income <= 84481:
            rate = 6.5
        elif income <= 92970:
            rate = 7
        elif income <= 99069:
            rate = 7.5
        else:
            rate = 8
    elif year == 2016:
        if income <= 54125:
            rate = 0
        elif income <= 60292:
            rate = 4
        elif income <= 66456:
            rate = 4.5
        elif income <= 69949:
            rate = 5
        elif income <= 75190:
            rate = 5.5
        elif income <= 81432:
            rate = 6
        elif income <= 85718:
            rate = 6.5
        elif income <= 94331:
            rate = 7
        elif income <= 100519:
            rate = 7.5
        else:
            rate = 8
    elif year == 2017:
        if income <= 54868:
            rate = 0
        elif income <= 61119:
            rate = 4
        elif income <= 67368:
            rate = 4.5
        elif income <= 70909:
            rate = 5
        elif income <= 76222:
            rate = 5.5
        elif income <= 82550:
            rate = 6
        elif income <= 86894:
            rate = 6.5
        elif income <= 95626:
            rate = 7
        elif income <= 101899:
            rate = 7.5
        else:
            rate = 8
    elif year == 2018:
        if income <= 55873:
            rate = 0
        elif income <= 62238:
            rate = 4
        elif income <= 68602:
            rate = 4.5
        elif income <= 72207:
            rate = 5
        elif income <= 77618:
            rate = 5.5
        elif income <= 84062:
            rate = 6
        elif income <= 88486:
            rate = 6.5
        elif income <= 97377:
            rate = 7
        elif income <= 103765:
            rate = 7.5
        else:
            rate = 8
    elif year == 2019:
        if income <= 51956:
            rate = 0
        elif income <= 57729:
            rate = 2
        elif income <= 64306:
            rate = 4
        elif income <= 70881:
            rate = 4.5
        elif income <= 74607:
            rate = 5
        elif income <= 80197:
            rate = 5.5
        elif income <= 86855:
            rate = 6
        elif income <= 91425:
            rate = 6.5
        elif income <= 100613:
            rate = 7
        elif income <= 107213:
            rate = 7.5
        else:
            rate = 8
    elif year == 2020:
        if income <= 45880:
            rate = 0
        elif income <= 52973:
            rate = 1
        elif income <= 56151:
            rate = 2
        elif income <= 59521:
            rate = 2.5
        elif income <= 63092:
            rate = 3
        elif income <= 66877:
            rate = 3.5
        elif income <= 70890:
            rate = 4
        elif income <= 75144:
            rate = 4.5
        elif income <= 79652:
            rate = 5
        elif income <= 84432:
            rate = 5.5
        elif income <= 89498:
            rate = 6
        elif income <= 94868:
            rate = 6.5
        elif income <= 100560:
            rate = 7
        elif income <= 106593:
            rate = 7.5
        elif income <= 112989:
            rate = 8
        elif income <= 119769:
            rate = 8.5
        elif income <= 126955:
            rate = 9
        elif income <= 134572:
            rate = 9.5
        else:
            rate = 10
    elif year == 2021:
        if income <= 46619:
            rate = 0
        elif income <= 53826:
            rate = 1
        elif income <= 57055:
            rate = 2
        elif income <= 60479:
            rate = 2.5
        elif income <= 64108:
            rate = 3
        elif income <= 67954:
            rate = 3.5
        elif income <= 72031:
            rate = 4
        elif income <= 76354:
            rate = 4.5
        elif income <= 80935:
            rate = 5
        elif income <= 85792:
            rate = 5.5
        elif income <= 90939:
            rate = 6
        elif income <= 96396:
            rate = 6.5
        elif income <= 102179:
            rate = 7
        elif income <= 108309:
            rate = 7.5
        elif income <= 114707:
            rate = 8
        elif income <= 121698:
            rate = 8.5
        elif income <= 128999:
            rate = 9
        elif income <= 136739:
            rate = 9.5
        else:
            rate = 10
    return rate/100

# Function that tells you the required HELP payment
def HELPpayment(income, year):
    rate = HELPrate(income, year)
    repayment = rate*income
    repayment = math.floor(repayment)
    return repayment
