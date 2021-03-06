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
#include <memory.h>

#include "conio.h"
#include "AIPlayer.h"

using namespace conio;

typedef struct Location {
    int row;
    int col;
} Location;

static Location attackPattern[32];
typedef struct Neighbors {
    Location data[4];
} Neighbors;

enum Mode {
    HUNT = 1, TARGET = 2, ATTACKING = 3
};
int attackPatternAcc = 0;

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

Location AIPlayer::getRandomLocation() {
  while (true) {
    Location loc;
    loc.row = random() % MAX_BOARD_SIZE;
    loc.col = random() % MAX_BOARD_SIZE;
    if (this->isValidMove(loc.row, loc.col))
      return loc;
  }
}

Neighbors AIPlayer::findNearestValidNeighbors(int row, int col) {
  Neighbors neighbors;
  Location loc;
  loc.row = 0;
  loc.col = 0;
  neighbors.data[0] = loc;
  neighbors.data[1] = loc;
  neighbors.data[2] = loc;
  neighbors.data[3] = loc;

  if ((row + 1 < MAX_BOARD_SIZE) && board[row + 1][col] == WATER) {
    Location loc;
    loc.row = row + 1;
    loc.col = col;
    neighbors.data[0] = loc;
  }
  if ((col + 1 < MAX_BOARD_SIZE) && board[row][col + 1] == WATER) {
    Location loc;
    loc.row = row;
    loc.col = col + 1;
    neighbors.data[2] = loc;
  }
  if ((row - 1 > -1) && board[row - 1][col] == WATER) {
    Location loc;
    loc.row = row - 1;
    loc.col = col;
    neighbors.data[1] = loc;
  }
  if ((col - 1 > -1) && board[row][col - 1] == WATER) {
    Location loc;
    loc.row = row;
    loc.col = col - 1;
    neighbors.data[3] = loc;
  }

  return neighbors;
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

bool AIPlayer::isValidMove(int row, int col) {
  if ((row < MAX_BOARD_SIZE) && (col < MAX_BOARD_SIZE) && (row > -1) && (col > -1) &&
      (this->board[row][col] == WATER)) {
    return true;
  } else {
    return false;
  }
}

Location AIPlayer::findMax() {
  Location loc;
  int localMax = -1000;
  for (int r = 0; r < MAX_BOARD_SIZE; r++) {
    for (int c = 0; c < MAX_BOARD_SIZE; c++) {
      if ((this->calculateWeightedAverage(r, c) > localMax) && board[r][c] == WATER) {
        localMax = calculateWeightedAverage(r, c);
        loc.row = r;
        loc.col = c;
      }
    }
  }
  return loc;
}

void AIPlayer::getDirectionForAttack(int lastRow, int lastCol, int currentRow, int currentCol) {
  if (lastRow == currentRow) {
    attackDirection = Horizontal;
  } else {
    if (lastCol == currentCol) {
      attackDirection = Vertical;
    } else {
      mode = HUNT;
    }
  }
}

Location AIPlayer::getTargetLocation(int row, int col) {
  Neighbors neighbors = findNearestValidNeighbors(row, col);
  int i = 0;

  while (true) {
    if (this->isValidMove(neighbors.data[i].row, neighbors.data[i].col) && this->goodShot(neighbors.data[i])) {
      Location result;
      result.row = neighbors.data[i].row;
      result.col = neighbors.data[i].col;
      return result;
    }
    i++;
  }
}

Location AIPlayer::getAttackingLocation() {
  if (attackDirection == Horizontal) {
    if (this->isValidMove(lastRow, lastCol - 1)) {
      Location result;
      result.row = lastRow;
      result.col = lastCol - 1;
      return result;
    } else {
      if (this->isValidMove(lastRow, lastCol + 1)) {
        Location result;
        result.row = lastRow;
        result.col = lastCol + 1;
        return result;
      } else {
        //go back to first strike
        if (this->board[lastRow][lastCol + 1] == MISS) {
          Location result;
          result.row = firstHitRow;
          result.col = firstHitCol - 1;
          return result;
        } else {
          if (this->board[lastRow][lastCol - 1] == MISS) {
            Location result;
            result.row = firstHitRow;
            result.col = firstHitCol + 1;
            return result;
          }
        }
      }
    }
  } else {
    if (this->isValidMove(lastRow - 1, lastCol)) {
      Location result;
      result.row = lastRow - 1;
      result.col = lastCol;
      return result;
    } else {
      if (this->isValidMove(lastRow + 1, lastCol)) {
        Location result;
        result.row = lastRow + 1;
        result.col = lastCol;
        return result;
      } else {
        //go back to first strike
        if (this->board[lastRow + 1][lastCol] == MISS) {
          Location result;
          result.row = firstHitRow - 1;
          result.col = firstHitCol;
          return result;
        } else {
          if (this->board[lastRow - 1][lastCol] == MISS) {
            Location result;
            result.row = firstHitRow + 1;
            result.col = firstHitCol;
            return result;
          }
        }
      }
    }
  }
}

bool AIPlayer::danglingHit() {
  for (int r = 0; r < MAX_BOARD_SIZE; r++) {
    for (int c = 0; c < MAX_BOARD_SIZE; c++) {
      if (this->board[r][c] == HIT) {
        return true;
      }
    }
  }
  return false;
}

Location AIPlayer::getDanglingHitLocation() {
  Location loc;
  for (int r = 0; r < MAX_BOARD_SIZE; r++) {
    for (int c = 0; c < MAX_BOARD_SIZE; c++) {
      if (this->board[r][c] == HIT) {
        loc.row = r;
        loc.col = c;
        return loc;
      }
    }
  }
}

//Pattern Attacking
//Location loc = attackPattern[attackPatternAcc];
//if (isValidMove(loc.row,loc.col)){
//Message result(SHOT, loc.row, loc.col, "Bang", None, 1);
//}
//attackPatternAcc++;

//Random Attacking
//Location loc = this->getRandomLocation();
//if (this->isValidMove(loc.row, loc.col)) {
//Message result(SHOT, loc.row, loc.col, "Bang", None, 1);
//return result;
//}

bool AIPlayer::goodShot(Location loc){
  if(isValidMove(loc.row, loc.col) && board[loc.row][loc.col] != WATER) return false;

  int vertShots = 1;
  int horzShots = 1;

  for(int r = loc.row - 1; r >= 0; r--){
    if (board[r][loc.col] == MISS || board[r][loc.col] == KILL){
      break;
    } else {
      vertShots++;
    }
  }

  for(int r = loc.row + 1; r < MAX_BOARD_SIZE; r++){
    if(board[r][loc.col] == MISS || board[r][loc.col] == KILL){
      break;
    } else {
      vertShots++;
    }
  }

  for(int c = loc.col - 1; c >= 0; c--){
    if(board[loc.row][c] == MISS || board[loc.row][c] == KILL){
      break;
    } else {
      horzShots++;
    }
  }

  for(int c = loc.col + 1; c < MAX_BOARD_SIZE; c++){
    if(board[loc.row][c] == MISS || board[loc.row][c] == KILL){
      break;
    } else {
      horzShots++;
    }
  }

  if(vertShots >= 3){
    return true;
  }
  if(horzShots >= 3){
    return true;
  }
  return false;
}

Message AIPlayer::getMove() {

  static Location attackPattern[32];
  Location loc;
  loc.row = 4;
  loc.col = 4;
  attackPattern[0] = loc;
  loc.row = 5;
  loc.col = 3;
  attackPattern[1] = loc;
  loc.row = 5;
  loc.col = 6;
  attackPattern[2] = loc;
  loc.row = 6;
  loc.col = 5;
  attackPattern[3] = loc;
  loc.row = 2;
  loc.col = 3;
  attackPattern[4] = loc;
  loc.row = 4;
  loc.col = 1;
  attackPattern[5] = loc;
  loc.row = 0;
  loc.col = 8;
  attackPattern[6] = loc;
  loc.row = 2;
  loc.col = 9;
  attackPattern[7] = loc;
  loc.row = 8;
  loc.col = 0;
  attackPattern[8] = loc;
  loc.row = 9;
  loc.col = 2;
  attackPattern[9] = loc;
  loc.row = 0;
  loc.col = 2;
  attackPattern[10] = loc;
  loc.row = 2;
  loc.col = 0;
  attackPattern[11] = loc;
  loc.row = 8;
  loc.col = 9;
  attackPattern[12] = loc;
  loc.row = 9;
  loc.col = 8;
  attackPattern[13] = loc;
  loc.row = 6;
  loc.col = 8;
  attackPattern[14] = loc;
  loc.row = 8;
  loc.col = 6;
  attackPattern[15] = loc;
  loc.row = 2;
  loc.col = 6;
  attackPattern[16] = loc;
  loc.row = 3;
  loc.col = 8;
  attackPattern[17] = loc;
  loc.row = 7;
  loc.col = 4;
  attackPattern[18] = loc;
  loc.row = 0;
  loc.col = 5;
  attackPattern[19] = loc;
  loc.row = 7;
  loc.col = 1;
  attackPattern[20] = loc;
  loc.row = 9;
  loc.col = 5;
  attackPattern[21] = loc;
  loc.row = 5;
  loc.col = 0;
  attackPattern[22] = loc;
  loc.row = 3;
  loc.col = 5;
  attackPattern[23] = loc;
  loc.row = 5;
  loc.col = 9;
  attackPattern[24] = loc;
  loc.row = 3;
  loc.col = 2;
  attackPattern[25] = loc;
  loc.row = 8;
  loc.col = 3;
  attackPattern[26] = loc;
  loc.row = 6;
  loc.col = 2;
  attackPattern[27] = loc;
  loc.row = 4;
  loc.col = 7;
  attackPattern[28] = loc;
  loc.row = 1;
  loc.col = 1;
  attackPattern[29] = loc;
  loc.row = 1;
  loc.col = 4;
  attackPattern[30] = loc;
  loc.row = 7;
  loc.col = 7;
  attackPattern[31] = loc;
  loc.row = 1;
  loc.col = 7;
  attackPattern[32] = loc;

  if (mode == HUNT) {
    while (true) {
      Location loc = attackPattern[this->attackPatternAcc];
      if(goodShot(loc)){
        if (isValidMove(loc.row, loc.col)) {
          Message result(SHOT, loc.row, loc.col, "Bang", None, 1);
          return result;
        }
      }
      this->attackPatternAcc++;
    }
  } else {
    if (mode == TARGET) {
      Location target = this->getTargetLocation(lastRow, lastCol);
      Message result(SHOT, target.row, target.col, "Bang", None, 1);
      return result;
    } else {
      if (mode == ATTACKING) {
        Location attack = this->getAttackingLocation();

        if (this->isValidMove(attack.row, attack.col)) {
          Message result(SHOT, attack.row, attack.col, "Bang", None, 1);
          return result;
        } else {
          Location target;
          if (this->danglingHit()) {
            target = this->getDanglingHitLocation();
            target = this->getTargetLocation(target.row, target.col);
          } else {
            target = this->getTargetLocation(firstHitRow, firstHitCol);
          }
          Message result(SHOT, target.row, target.col, "Bang", None, 1);
          return result;
        }
      }
    }
  }
}

int AIPlayer::calculateWeightedAverage(int row, int col) {
  return (this->enemyHeatmapThisRound[row][col] * 2) +
         (this->enemyHeatmapThisGame[row][col] * 1) +
         (this->searchPatternHeatmap[row][col] * 1);
}

void AIPlayer::newRound() {
  /* DumbPlayer is too simple to do any inter-round learning. Smarter players
   * reinitialize any round-specific data structures here.
   */
  this->lastRow = 0;
  this->lastCol = 0;
  this->numShipsPlaced = 0;
  this->mode = HUNT;
  this->attackPatternAcc = 0;
  this->initializeBoard();
}

Message AIPlayer::placeShip(int length) {
  this->shipSizes[numShipsPlaced] = length;
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

void AIPlayer::update(Message msg) {
  switch (msg.getMessageType()) {
    case HIT:
      this->board[msg.getRow()][msg.getCol()] = msg.getMessageType();
      if (this->mode == HUNT) {
        this->firstHitRow = msg.getRow();
        this->firstHitCol = msg.getCol();
      }
      this->mode = TARGET;
      if (this->board[lastRow][lastCol] == HIT) {
        this->mode = ATTACKING;
        this->getDirectionForAttack(lastRow, lastCol, msg.getRow(), msg.getCol());
      }
      this->lastRow = msg.getRow();
      this->lastCol = msg.getCol();
      break;
    case KILL:
      this->board[msg.getRow()][msg.getCol()] = msg.getMessageType();
      this->mode = HUNT;
      if(this->danglingHit())
        this->mode = ATTACKING;
      break;
    case MISS:
      this->board[msg.getRow()][msg.getCol()] = msg.getMessageType();
      if(this->danglingHit())
        this->mode = ATTACKING;
      break;
    case WIN:
    case LOSE:
    case TIE:
      break;
    case OPPONENT_SHOT:
      break;
  }
}

