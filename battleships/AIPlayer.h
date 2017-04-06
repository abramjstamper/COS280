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

    bool canPlaceShip(int row, int col, int direction, int length);

    void markShip(int row, int col, int direction, int length);

    bool validMove(int row, int col);

    int* checkHeatMap();

    void updateHeatMap(int row, int col);

    int lastRow;
    int lastCol;
    bool lastShotWasHit;
    int moveNumber;
    int numShipsPlaced;
    int enemyHeatmapThisRound[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    int enemyHeatmapThisGame[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    char myShipBoard[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    char board[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
};

#endif
