/**

This is code for a simulated Paper Scissors Rock competition.

The program is invoked on the command line like this:

./pcr_runner p1.lua p2.lua 3

The first two are lua programs that implement to doRound function.
The 3rd parameter is how many rounds you want the competition to run.

Output should look like this:

Welcome to the paper scissors rock competition runner

Always rock initalized
Mirror initalized
Number of rounds: 3
P1=rock P2=paper.  Player 2 wins.
P1=rock P2=rock.  Tie.
P1=rock P2=rock.  Tie.
Final Score: P1: 0 P2: 1 Ties: 2

 */

#include <lua.h>                                
#include <lauxlib.h>                            
#include <lualib.h>                             
                                                

#include <stdlib.h>                             
#include <stdio.h>                              

const int INITIAL_PLAY = 0;
const int PAPER = 1;
const int SCISSORS = 2;
const int ROCK = 3;

int whoWins(int p1Choice, int p2Choice) {
    if(p1Choice == p2Choice) return 0;
    if(p1Choice == PAPER && p2Choice == ROCK) return 1;
    if(p1Choice == SCISSORS && p2Choice == PAPER) return 1;
    if(p1Choice == ROCK && p2Choice == SCISSORS) return 1;
    return 2;
}

char* toString(int play) {
    switch (play) {
        case 1:
            return "paper";
        case 2:
            return "scissors";
        case 3:
            return "rock";
        default:
            return "illegal move";
        }
}

int main(int argc, char *argv[])
{

    printf("Welcome to the paper scissors rock competition runner\n\n");
    
    if(argc < 3) {
        printf("Usage: pcr_runner PLAYER1CODE PLAYER2CODE ROUNDS\n");
        exit(1);
    }


    // You'll probably want to put some lua initialization code here

    int numRounds = atoi(argv[3]);
    printf("Number of rounds: %d\n",numRounds);

    int i;
    int lastP1 = INITIAL_PLAY;
    int lastP2 = INITIAL_PLAY;
    int wins[] = {0,0,0};
    for(i = 0; i < numRounds; i++) {

        lastP1 = 0; //these variables should be set by your calls to
                    //lua
        lastP2 = 0;

        int winner = whoWins(lastP1,lastP2);
        wins[winner]++;
        if(winner == 0)
            printf("P1=%s P2=%s.  Tie.\n",toString(lastP1), toString(lastP2));
        else
            printf("P1=%s P2=%s.  Player %d wins.\n",toString(lastP1), toString(lastP2),winner);
    }
    printf("Final Score: P1: %d P2: %d Ties: %d\n",wins[1],wins[2],wins[0]);

    return 0;
}
