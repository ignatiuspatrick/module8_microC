int x = 0;
int y = 10;
boolean z = false;

function int doStuff(int b, int d, boolean n) {
    b = 44;
    while (n == false) {
        d = d + 3;
        if (d >= 20) {
            n = true;
        }
    }
    return d;
}

if (z == false) {
    int c = doStuff(x, y, z);
}
