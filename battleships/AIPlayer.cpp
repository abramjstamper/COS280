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
      this->enemyHeatmapThisGame[row][col] = 0;
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
      this->myShipBoard[row][col] = WATER;
      this->enemyHeatmapThisRound[row][col] = 0;
    }
  }
}

void AIPlayer::copyEnemyShipLocation() {
  for (int i = 0; i < MAX_BOARD_SIZE; i++) {
    for (int j = 0; j < MAX_BOARD_SIZE; j++) {
      enemyHeatmapThisGame[i][j] += enemyHeatmapThisRound[i][j];
    }
  }
}

/**
 * debug function to print the placement of ships
 */
void AIPlayer::printBoard(int board[MAX_BOARD_SIZE][MAX_BOARD_SIZE]) {
  printf("   0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n");
  for (int r = 0; r < MAX_BOARD_SIZE; r++) {
    printf("%i  ", r);
    for (int c = 0; c < MAX_BOARD_SIZE; c++) {
      printf("%i | ", board[r][c]);
    }
    printf("\n  ---------------------------------------\n");
  }
  printf("\n");
  printf("\n");
  printf("\n");
}

void AIPlayer::printBoard(char board[MAX_BOARD_SIZE][MAX_BOARD_SIZE]) {
  printf("   0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n");
  for (int r = 0; r < MAX_BOARD_SIZE; r++) {
    printf("%i  ", r);
    for (int c = 0; c < MAX_BOARD_SIZE; c++) {
      printf("%c | ", board[r][c]);
    }
    printf("\n  ---------------------------------------\n");
  }
  printf("\n");
  printf("\n");
  printf("\n");
}

/**
 * helper functions
 */

void AIPlayer::addShot(int row, int col){
  this->updateHeatMap(row, col);
  this->lastShotWasHit = true;
}

void AIPlayer::updateHeatMap(int row, int col){
  this->enemyHeatmapThisRound[row + 1][col] += 1;
  this->enemyHeatmapThisRound[row][col + 1] += 1;
  this->enemyHeatmapThisRound[row - 1][col] += 1;
  this->enemyHeatmapThisRound[row][col - 1] += 1;

  if(this->board[row+1][col] == HIT) {
    this->enemyHeatmapThisRound[row + 2][col] += 2;
    this->enemyHeatmapThisRound[row][col+1] -= 2;
    this->enemyHeatmapThisRound[row][col-1] -= 2;
    this->enemyHeatmapThisRound[row+1][col+1] -= 2;
    this->enemyHeatmapThisRound[row+1][col-1] -= 2;
  }

  if(this->board[row-1][col] == HIT) {
    this->enemyHeatmapThisRound[row - 2][col] += 2;
    this->enemyHeatmapThisRound[row][col + 1] -= 2;
    this->enemyHeatmapThisRound[row][col - 1] -= 2;
    this->enemyHeatmapThisRound[row - 1][col + 1] -= 2;
    this->enemyHeatmapThisRound[row - 1][col - 1] -= 2;
  }

  if(this->board[row][col+1] == HIT) {
    this->enemyHeatmapThisRound[row][col - 1] += 2;
    this->enemyHeatmapThisRound[row + 1][col] -= 2;
    this->enemyHeatmapThisRound[row - 1][col] -= 2;
    this->enemyHeatmapThisRound[row - 1][col + 1] -= 2;
    this->enemyHeatmapThisRound[row + 1][col + 1] -= 2;
  }

  if(this->board[row][col-1] == HIT) {
    this->enemyHeatmapThisRound[row][col + 1] += 2;
    this->enemyHeatmapThisRound[row + 1][col] -= 2;
    this->enemyHeatmapThisRound[row - 1][col] -= 2;
    this->enemyHeatmapThisRound[row - 1][col - 1] -= 2;
    this->enemyHeatmapThisRound[row + 1][col - 1] -= 2;
  }

}

void AIPlayer::markShip(int row, int col, int direction, int length) {
  if (direction == 1) {
    for (int i = col; i < (col + length); i++) {
      this->myShipBoard[row][i] = SHIP;
    }
  } else {
    if (direction == 2) {
      for (int i = row; i < (row + length); i++) {
        this->myShipBoard[i][col] = SHIP;
      }
    }
  }
}

bool AIPlayer::canPlaceShip(int row, int col, int direction, int length) {
  if (direction == 1) {
    if ((col + length) >= MAX_BOARD_SIZE) {
      return false;
    } else {
      for (int i = col; i < (col + length); i++) {
        if (this->myShipBoard[row][i] == SHIP)
          return false;
      }
    }
  } else {
    if ((row + length) >= MAX_BOARD_SIZE) {
      return false;
    } else {
      for (int i = row; i < (row + length); i++) {
        if (this->myShipBoard[i][col] == SHIP)
          return false;
      }
    }
  }
  return true;
}

bool AIPlayer::validMove(int row, int col){
  if(board[row][col] == WATER && (row < MAX_BOARD_SIZE) && (col < MAX_BOARD_SIZE)){
    return true;
  } else {
    return false;
  }
}

int* AIPlayer::checkHeatMap(){
  int localMax = -1000;
  int row = 0;
  int col = 0;
  for(int r = 0; r < MAX_BOARD_SIZE; r++){
    for(int c = 0; c < MAX_BOARD_SIZE; c++){
      if((enemyHeatmapThisRound[r][c] > localMax) && board[r][c] == WATER){
        localMax = enemyHeatmapThisRound[r][c];
        row = r;
        col = c;
      }
    }
  }
  if(localMax == 0){
    row = random() % 10;
    col = random() % 10;
    localMax = enemyHeatmapThisRound[row][col];
  }

  int* buffer = (int *) malloc(sizeof(int) * 3);
  buffer[0] = row;
  buffer[1] = col;
  buffer[2] = localMax;
  return buffer;
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
  int row = 0;
  int col = 0;

  while(true){
    if(moveNumber > 8){
      int* buffer = this->checkHeatMap();
      row = buffer[0];
      col = buffer[1];
      free(buffer);
    } else {
      row = random() % 4 + 3;
      col = random() % 4 + 3;
    }
    if(lastShotWasHit){
      //do something
    }

    if(this->validMove(row, col)){
      lastRow = row;
      lastCol = col;
      this->moveNumber++;
      Message result(SHOT, row, col, "Bang", None, 1);
      return result;
    }
  }
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
  this->moveNumber = 0;
  this->lastShotWasHit = false;

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
Message AIPlayer::placeShip(int length) {
  char shipName[10];
  int row = 0;
  int col = 0;
  int corner = 0;
  int ZERO_BASED_MAX_BOARD_SIZE = 9;
  int direction = random() % 2 + 1;
  // Create ship names each time called: Ship0, Ship1, Ship2, ...
  snprintf(shipName, sizeof shipName, "Ship%d", numShipsPlaced);

  if (numShipsPlaced < 1) {
    corner = random() % 3 + 1;
  }
//  printf("Board Size: %i   -   Corner: %i\n", MAX_BOARD_SIZE, corner);
  while (true) {
    switch (corner) {
      case 1:
        col = 0;
        row = 0;
        break;
      case 2:
        if (direction == 1) {
          col = ZERO_BASED_MAX_BOARD_SIZE - length;
          row = 0;
        } else {
          col = ZERO_BASED_MAX_BOARD_SIZE;
          row = 0;
        }
        break;
      case 3:
        if (direction == 1) {
          col = ZERO_BASED_MAX_BOARD_SIZE - length;
          row = ZERO_BASED_MAX_BOARD_SIZE;
        } else {
          col = ZERO_BASED_MAX_BOARD_SIZE;
          row = ZERO_BASED_MAX_BOARD_SIZE - length;
        }
        break;
      case 4:
        if (direction == 1) {
          col = 0;
          row = ZERO_BASED_MAX_BOARD_SIZE;
        } else {
          col = 0;
          row = ZERO_BASED_MAX_BOARD_SIZE - length;
        }
        break;
      default:
        if (direction == 1) {
          col = random() % (MAX_BOARD_SIZE - length + 1);
          row = random() % MAX_BOARD_SIZE;
        } else {
          col = random() % MAX_BOARD_SIZE;
          row = random() % (MAX_BOARD_SIZE - length + 1);
        }
    }
    if (canPlaceShip(row, col, direction, length)) {
      markShip(row, col, direction, length);
      Message response(PLACE_SHIP, row, col, shipName, Direction(direction), length);
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
      //weighs heavier when there's a kill
      this->addShot(msg.getRow(), msg.getCol());
    case MISS:
      this->board[msg.getRow()][msg.getCol()] = msg.getMessageType();
      break;
    case WIN:
    case LOSE:
    case TIE:
      this->copyEnemyShipLocation();
      //this->printBoard(this->enemyHeatmapThisGame);
      break;
    case OPPONENT_SHOT:
      break;
  }
}

