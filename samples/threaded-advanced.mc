int i = 0;
int x = 0;


function int g () {
    boolean y = true;
    fork (i, x) {
        int a = 0;

        fork (a) {
            lock a;
            a = 10;
            unlock a;
        } {
            lock a;
            a = 50;
            unlock a;
        }
        x = a;
    } {
        int a = 0;
        while (a < 1000) {
            x = x + 1;
            a = a + 1;
        }
    }

    return x;
}

int z = g();