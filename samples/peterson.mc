int turn = 0;
boolean t1 = false;
boolean t2 = false;
int i = 0;

fork (i, turn, t1, t2) {
    t1 = true;

    while (t2 && turn == 1) {
        i = i + 1;
    }
    i = 22;

    t1 = false;
    turn = 0;
} {
    t2 = true;

    while (t1 && turn == 0) {
        i = i + 1;
    }
    i = 77;

    t2 = false;
    turn = 0;
}
i = i;