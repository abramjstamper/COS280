cmake_minimum_required(VERSION 3.6)
project(battleships)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    AIContest.cpp
    AIContest.h
    AITester.h
    BoardV3.h
    CleanPlayerV2.h
    conio.cpp
    conio.h
    contest.cpp
    defines.h
    DumbPlayerV2.cpp
    DumbPlayerV2.h
    LearningGambler2.h
    Makefile
    Message.cpp
    Message.h
    OrigGamblerPlayerV2.h
    PlayerV2.cpp
    PlayerV2.h
    binaries.tar)

add_executable(battleships ${SOURCE_FILES})