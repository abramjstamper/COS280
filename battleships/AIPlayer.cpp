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

typedef struct Neighbors {
    Location data[4];
} Neighbors;

enum Mode {
    HUNT = 1, TARGET = 2, ATTACKING = 3
};

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

    static int searchPatternHeatmap1[MAX_BOARD_SIZE][MAX_BOARD_SIZE] = {
            {1, 0, 1, 0, 1, 0, 1, 0, 1, 0},
            {0, 2, 0, 2, 0, 2, 0, 2, 0, 1},
            {1, 0, 3, 0, 3, 0, 3, 0, 2, 0},
            {0, 2, 0, 4, 0, 4, 0, 3, 0, 1},
            {1, 0, 3, 0, 5, 0, 4, 0, 2, 0},
            {0, 2, 0, 4, 0, 5, 0, 3, 0, 1},
            {1, 0, 3, 0, 4, 0, 4, 0, 2, 0},
            {0, 2, 0, 3, 0, 3, 0, 3, 0, 1},
            {1, 0, 2, 0, 2, 0, 2, 0, 2, 0},
            {0, 1, 0, 1, 0, 1, 0, 1, 0, 1}
    };

    memcpy(searchPatternHeatmap, searchPatternHeatmap1, MAX_BOARD_SIZE * MAX_BOARD_SIZE * sizeof(int));
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

/**
 * helper functions
 */

void AIPlayer::addShot(int row, int col) {
    this->updateHeatMap(row, col);
}

void AIPlayer::resetAfterKill(int row, int col) {
    if (row - 1 > -1)
        this->enemyHeatmapThisRound[row - 1][col] = 0;
    if (col - 1 > -1)
        this->enemyHeatmapThisRound[row][col - 1] = 0;
    if (row + 1 < MAX_BOARD_SIZE)
        this->enemyHeatmapThisRound[row + 1][col] = 0;
    if (col + 1 < MAX_BOARD_SIZE)
        this->enemyHeatmapThisRound[row][col + 1] = 0;
}

void AIPlayer::missShot(int row, int col) {
    //this->lastShotWasHit = false;
    this->enemyHeatmapThisRound[row][col] = -10;
    if (row - 1 > -1)
        this->enemyHeatmapThisRound[row - 1][col] -= 1;
    if (col - 1 > -1)
        this->enemyHeatmapThisRound[row][col - 1] -= 1;
    if (row + 1 < MAX_BOARD_SIZE)
        this->enemyHeatmapThisRound[row + 1][col] -= 1;
    if (col + 1 < MAX_BOARD_SIZE)
        this->enemyHeatmapThisRound[row][col + 1] -= 1;
}

void AIPlayer::updateHeatMap(int row, int col) {

    //Neighbors
//  if(this->lastShotWasHit) {
//    if (row + 1 < MAX_BOARD_SIZE)
//      this->enemyHeatmapThisRound[row + 1][col] += 1;
//    if (col + 1 < MAX_BOARD_SIZE)
//      this->enemyHeatmapThisRound[row][col + 1] += 1;
//    if (row - 1 > -1)
//      this->enemyHeatmapThisRound[row - 1][col] += 1;
//    if (col - 1 > -1)
//      this->enemyHeatmapThisRound[row][col - 1] += 1;
//  }

//    //vertical
//    if (this->board[row + 1][col] == HIT) {
//        if (row + 2 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row + 2][col] += 3;
//        if (col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row][col + 1] -= 1;
//        if (col - 1 > -1)
//            this->enemyHeatmapThisRound[row][col - 1] -= 1;
//        if (row + 1 < MAX_BOARD_SIZE && col + 1 > MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row + 1][col + 1] -= 1;
//        if (row + 1 < MAX_BOARD_SIZE && col - 1 > -1)
//            this->enemyHeatmapThisRound[row + 1][col - 1] -= 1;
//        if (row - 1 > -1)
//            this->enemyHeatmapThisRound[row - 1][col] += 2;
//    }
//
//    if (this->board[row - 1][col] == HIT) {
//        if (row - 2 > -1)
//            this->enemyHeatmapThisRound[row - 2][col] += 3;
//        if (col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row][col + 1] -= 1;
//        if (col - 1 > -1)
//            this->enemyHeatmapThisRound[row][col - 1] -= 1;
//        if (row - 1 > -1 && col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row - 1][col + 1] -= 1;
//        if (row - 1 > -1 && col - 1 > -1)
//            this->enemyHeatmapThisRound[row - 1][col - 1] -= 1;
//        if (row + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row + 1][col] += 2;
//    }
//
//    //horizontal
//    if (this->board[row][col + 1] == HIT) {
//        if (col - 1 > -1)
//            this->enemyHeatmapThisRound[row][col - 1] += 3;
//        if (row + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row + 1][col] -= 1;
//        if (row - 1 > -1)
//            this->enemyHeatmapThisRound[row - 1][col] -= 1;
//        if (row - 1 > -1 && col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row - 1][col + 1] -= 1;
//        if (row + 1 < MAX_BOARD_SIZE && col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row + 1][col + 1] -= 1;
//        if (col - 1 > -1)
//            this->enemyHeatmapThisRound[row][col - 1] += 2;
//    }
//
//    if (this->board[row][col - 1] == HIT) {
//        if (col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row][col + 1] += 3;
//        if (row + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row + 1][col] -= 1;
//        if (row - 1 > -1)
//            this->enemyHeatmapThisRound[row - 1][col] -= 1;
//        if (row - 1 > -1 && col - 1 > -1)
//            this->enemyHeatmapThisRound[row - 1][col - 1] -= 1;
//        if (row + 1 < MAX_BOARD_SIZE && col - 1 > -1)
//            this->enemyHeatmapThisRound[row + 1][col - 1] -= 1;
//        if (col + 1 < MAX_BOARD_SIZE)
//            this->enemyHeatmapThisRound[row][col + 1] += 2;
//    }

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

    if ((row + 1 < MAX_BOARD_SIZE) && board[row][col] != WATER) {
        Location loc;
        loc.row = row + 1;
        loc.col = col;
        neighbors.data[0] = loc;
    }
    if ((row - 1 > -1) && board[row][col] != WATER) {
        Location loc;
        loc.row = row - 1;
        loc.col = col;
        neighbors.data[1] = loc;
    }
    if ((col + 1 < MAX_BOARD_SIZE) && board[row][col] != WATER) {
        Location loc;
        loc.row = row;
        loc.col = col + 1;
        neighbors.data[2] = loc;
    }
    if ((col - 1 > -1) && board[row][col] != WATER) {
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

Location AIPlayer::getTargetLocation() {
    Neighbors neighbors = findNearestValidNeighbors(lastRow, lastCol);
    int i = 0;

    while (true) {
        if (this->isValidMove(neighbors.data[i].row, neighbors.data[i].col)) {
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


Message AIPlayer::getMove() {

    if (mode == HUNT) {
        while (true) {
            Location loc = this->getRandomLocation();

            if (this->isValidMove(loc.row, loc.col)) {
                Message result(SHOT, loc.row, loc.col, "Bang", None, 1);
                return result;
            }
        }
    } else {
        if (mode == TARGET) {
            Location target = this->getTargetLocation();
            Message result(SHOT, target.row, target.col, "Bang", None, 1);
            return result;
        } else {
            if (mode == ATTACKING) {
                Location attack = this->getAttackingLocation();

                if (this->isValidMove(attack.row, attack.col)) {
                    Message result(SHOT, attack.row, attack.col, "Bang", None, 1);
                    return result;
                } else {
                    Location DHLocation;
                    if(this->danglingHit()){
                        DHLocation = getDanglingHitLocation();
                        printf("\n\n\n\n\n\n\n\nblahblahblah\n%i %i", DHLocation.row,DHLocation.col );
                    }
                    printf("\n\n\n\n\n\n\n\nblahblahblah\n%i %i", DHLocation.row,DHLocation.col );

                    Location target = this->getTargetLocation();
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
            break;
        case MISS:
            this->board[msg.getRow()][msg.getCol()] = msg.getMessageType();
            break;
        case WIN:
        case LOSE:
        case TIE:
            break;
        case OPPONENT_SHOT:
            break;
    }
}

