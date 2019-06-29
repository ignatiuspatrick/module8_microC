int x = 1;
int y = 10;
boolean z = true;

function int doStuff() {
    boolean a = true;

    if (a == true) {
        int z = false;
    }

    int c = 0;

    while (z == false) {
        c = c + 4;
        if (c >= 10) {
            z = true;
        }
    }
    return c;
}
int x = 0;
if (z) {
    int c = doStuff();
}
