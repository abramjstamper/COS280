CXXFLAGS = -Wall -O0 -g
CXX = g++

CONTESTOBJECTS = AIContest.o BoardV3.o Message.o PlayerV2.o conio.o contest.o \
	AIPlayer.o CleanPlayerV2.o OrigGamblerPlayerV2.o LearningGambler2.o

contest: $(CONTESTOBJECTS)
	gcc-6 -o contest $(CONTESTOBJECTS)
	@echo "Contest binary is in 'contest'. Run as './contest'"

clean:
	rm -f contest $(CONTESTOBJECTS) $(TESTEROBJECTS)


contest.o: contest.cpp
contest.cpp: defines.h Message.cpp

tester.o: tester.cpp
tester.cpp: defines.h Message.cpp

Message.o: Message.cpp
Message.cpp: Message.h defines.h

conio.o: conio.cpp
conio.cpp: conio.h

AIContest.o: AIContest.cpp
AIContest.cpp: AIContest.h Message.h defines.h

# Players here
AIPlayer.o: AIPlayer.cpp Message.h
AIPlayer.cpp: AIPlayer.h defines.h PlayerV2.h conio.cpp

# CleanPlayerV2.o and other provided binaries are only available as a linkable Linux binary, not as source code.
CleanPlayerV2.o: 
	tar -xvf binaries.tar CleanPlayerV2.o
OrigGamblerPlayerV2.o:
	tar -xvf binaries.tar OrigGamblerPlayerV2.o
LearningGambler2.o:
	tar -xvf binaries.tar LearningGambler2.o
BoardV3.o:
	tar -xvf binaries.tar BoardV3.o
