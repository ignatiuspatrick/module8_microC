int x = 1;
int y = 10;
boolean z = true;

function int doStuff() {
    boolean a = true;

    if (a == true) {
        int z = 1;
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
if (z){
    int c = doStuff();
}
