int i = 0;
boolean x = false;
int g = 1;


function int y () {
    fork (i, x) {
        boolean z = true;
        if (z == true) {
            while (false) {
                lock i;
                i = 10;

                fork (i) {
                    i = 20;
                } {
                    int x = 0;
                    x = i;
                }
                unlock i;
            }
        }
    } {
        boolean y = true;
        x = y && false;
    }

    return 0;
}

x = true;