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
    void copyWhereEnemyShotThisRoundToThisGame();
    bool canPlaceShip(int row, int col, int direction, int length);
    void markShip(int row, int col, int direction, int length);
    int lastRow;
    int lastCol;
    int numShipsPlaced;
    int whereEnemyShotThisRound[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    int whereEnemyShotThisGame[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    char myShipBoard[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
    char board[MAX_BOARD_SIZE][MAX_BOARD_SIZE];
};

#endif
