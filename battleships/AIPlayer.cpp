/**
 * @brief DumbPlayer AI for battleships
 * @file AIPlayer.cpp
 * @author Stefan Brandle, Jonathan Geisler
 * @date September, 2004 Updated 2015 for multi-round play.
 *
 * This Battleships AI is very simple and does nothing beyond playing
 * a legal game. However, that makes it a good starting point for writing
 * a more sophisticated AI.
 *
 * The constructor
 */

#include <iostream>
#include <cstdio>
#include <cstdlib>

#include "conio.h"
#include "AIPlayer.h"

using namespace conio;

/**
 * @brief Constructor that initializes any inter-round data structures.
 * @param boardSize Indication of the size of the board that is in use.
 *
 * The constructor runs when the AI is instantiated (the object gets created)
 * and is responsible for initializing everything that needs to be initialized
 * before any of the rounds happen. The constructor does not get called 
 * before rounds; newRound() gets called before every round.
 */
AIPlayer::AIPlayer(int boardSize)
        : PlayerV2(boardSize) {
  // Could do any initialization of inter-round data structures here.
  for (int row = 0; row < boardSize; row++) {
    for (int col = 0; col < boardSize; col++) {
      this->whereEnemyShotThisGame[row][col] = 0;
    }
  }

}

/**
 * @brief Destructor placeholder.
 * If your code does anything that requires cleanup when the object is
 * destroyed, do it here in the destructor.
 */
AIPlayer::~AIPlayer() {}

/*
 * Private internal function that initializes a MAX_BOARD_SIZE 2D array of char to water.
 */
void AIPlayer::initializeBoard() {
  for (int row = 0; row < boardSize; row++) {
    for (int col = 0; col < boardSize; col++) {
      this->board[row][col] = WATER;
      this->whereEnemyShotThisRound[row][col] = 0;
    }
  }
}

void AIPlayer::copyWhereEnemyShotThisRoundToThisGame() {
  for (int i = 0; i < MAX_BOARD_SIZE; i++) {
    for (int j = 0; j < MAX_BOARD_SIZE; j++) {
      whereEnemyShotThisRound[i][j] += whereEnemyShotThisGame[i][j];
    }
  }
}


/**
 * @brief Specifies the AI's shot choice and returns the information to the caller.
 * @return Message The most important parts of the returned message are 
 * the row and column values. 
 *
 * See the Message class documentation for more information on the 
 * Message constructor.
 */
Message AIPlayer::getMove() {
  lastCol++;
  if (lastCol >= boardSize) {
    lastCol = 0;
    lastRow++;
  }
  if (lastRow >= boardSize) {
    lastCol = 0;
    lastRow = 0;
  }

  Message result(SHOT, lastRow, lastCol, "Bang", None, 1);
  return result;
}

/**
 * @brief Tells the AI that a new round is beginning.
 * The AI show reinitialize any intra-round data structures.
 */
void AIPlayer::newRound() {
  /* DumbPlayer is too simple to do any inter-round learning. Smarter players
   * reinitialize any round-specific data structures here.
   */
  this->lastRow = 0;
  this->lastCol = -1;
  this->numShipsPlaced = 0;

  this->initializeBoard();
}

/**
 * @brief Gets the AI's ship placement choice. This is then returned to the caller.
 * @param length The length of the ship to be placed.
 * @return Message The most important parts of the returned message are 
 * the direction, row, and column values. 
 *
 * The parameters returned via the message are:
 * 1. the operation: must be PLACE_SHIP 
 * 2. ship top row value
 * 3. ship top col value
 * 4. a string for the ship name
 * 5. direction Horizontal/Vertical (see defines.h)
 * 6. ship length (should match the length passed to placeShip)
 */
bool AIPlayer::canPlaceShip(int row, int col, int direction, int length) {
  if (direction == 1) {
    if ((row + length) > MAX_BOARD_SIZE) {
      return false;
    } else {
      for (int i = 0; i < length; i++) {
        if (this->myShipBoard[i][col] == SHIP)
          return false;
      }
    }
  } else {
    if ((col + length) > MAX_BOARD_SIZE) {
      return false;
    } else {
      for (int i = 0; i < length; i++) {
        if (this->myShipBoard[row][i] == SHIP)
          return false;
      }
    }
  }
  return true;
}

void AIPlayer::markShip(int row, int col, int direction, int length) {
  if (direction == 1) {
    for (int i = 0; i < length; i++) {
      this->myShipBoard[i][col] = SHIP;
    }
  } else {
    if (direction == 2) {
      for (int i = 0; i < length; i++) {
        this->myShipBoard[row][i] = SHIP;
      }
    }
  }
}

Message AIPlayer::placeShip(int length) {
  char shipName[10];
  int row = 0;
  int col = 0;
  int horizontal = 0;
  // Create ship names each time called: Ship0, Ship1, Ship2, ...
  snprintf(shipName, sizeof shipName, "Ship%d", numShipsPlaced);

  while (true) {
    horizontal = random() % 2 + 1;
    if (horizontal == 1) {
      col = random() % (MAX_BOARD_SIZE - length + 1);
      row = random() % MAX_BOARD_SIZE;
    } else {
      col = random() % MAX_BOARD_SIZE;
      row = random() % (MAX_BOARD_SIZE - length + 1);
    }
    if (canPlaceShip(row, col, horizontal, length)) {
      markShip(row, col, horizontal, length);
      Message response(PLACE_SHIP, row, col, shipName, Direction(horizontal), length);
      numShipsPlaced++;
      return response;
    }
  }
}

/**
 * @brief Updates the AI with the results of its shots and where the opponent is shooting.
 * @param msg Message specifying what happened + row/col as appropriate.
 */
void AIPlayer::update(Message msg) {
  switch (msg.getMessageType()) {
    case HIT:
    case KILL:
    case MISS:
      board[msg.getRow()][msg.getCol()] = msg.getMessageType();
      break;
    case WIN:
      break;
    case LOSE:
      break;
    case TIE:
      break;
    case OPPONENT_SHOT:
      whereEnemyShotThisRound[msg.getRow()][msg.getCol()] = 1;
      break;
  }
  copyWhereEnemyShotThisRoundToThisGame();
}

