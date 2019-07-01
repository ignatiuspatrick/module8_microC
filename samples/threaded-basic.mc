int i = 0;
int x = 0;


function int g () {
    boolean y = true;
    fork (i, x) {
        boolean z = true;
        if (z == true) {
            i = 50;
        }
    } {
        boolean z = true;
        if (z || true) {
            x = 30;
        }
    }

    return i;
}

int z = g();