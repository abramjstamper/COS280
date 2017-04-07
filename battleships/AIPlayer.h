/**
 * @author Stefan Brandle, Jonathan Geisler
 * @date September, 2004
 *
 * Please type in your name[s] here:
 *
 */

#ifndef AIPLAYER_H    // Double inclusion protection
#define AIPLAYER_H

using namespace std;

#include "PlayerV2.h"
#include "Message.h"
#include "defines.h"
#include <stack>

// DumbPlayer inherits from/extends PlayerV2

class AIPlayer : public PlayerV2 {
public:
    AIPlayer(int boardSize);

    ~AIPlayer();

    void newRound();

    Message placeShip(int length);

    Message getMove();

    void update(Message msg);

private:
    void initializeBoard();

    void copyEnemyShipLocation();

    void printBoard(int board[MAX_BOARD_SIZE][MAX_BOARD_SIZE]);

    void printBoard(char board[MAX_BOARD_SIZE][MAX_BOARD_SIZE]);

    void addShot(int row, int col);

    void missShot(int row, int col);

    void resetAfterKill(int row, int col);

    bool canPlaceShip(int row, int col, int direction, int length);

    void markShip(int row, int col, int direction, int length);

    bool validMove(int row, int col);

    int* checkHeatMap();

    void updateHeatMap(int row, int col);

    int calculateWeightedAverage(int row, int col);

    int lastRow;
    int lastCol;
    bool lastShotWasHit;
    int numShipsPlaced;
    const int searchPatternHeatmap[MAX_BOARD_SIZE][MAX_BOARD_SIZE] = {
            {1,0,1,0,1,0,1,0,1,0},
            {0,2,0,2,0,2,0,2,0,1},
            {1,0,3,0,3,0,3,0,2,0},
            {0,2,0,4,0,4,0,3,0,1},
            {1,0,3,0,5,0,4,0,2,0},
            {0,2,0,4,0,5,0,3,0,1},
            {1,0,3,0,4,0,4,0,2,0},
            {0,2,0,3,0,3,0,3,0,1},
            {1,0,2,0,2,0,2,0,2,0},
            {0,1,0,1,0,1,0,1,0,1}
    };
    int enemyHeatmapThisRound[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    int enemyHeatmapThisGame[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    char myShipBoard[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    char board[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    int buffer[3];
};

#endif
