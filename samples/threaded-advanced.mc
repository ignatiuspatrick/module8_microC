int i = 0;
int x = 0;


function int g () {
    boolean y = true;
    int a = 0;

    fork (a, x) {
        a = 33;
        int b = 0;
        fork (a, b) {
            lock a;
            a = 10;
            unlock a;
        } {
            lock a;
            a = 50;
            unlock a;

            int c = 0;
            fork (a, c) {
                lock a;
                a = 50;
                unlock a;
            } {
                lock a;
                a = 10;
                unlock a;
            }
        }
    } {
        while (a < 1000) {
            x = x + 1;
            a = a + 1;
        }
    }

    return x;
}

int z = g();