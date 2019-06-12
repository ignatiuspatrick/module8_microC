int x = 1;
int y = 10;
boolean z = true;


function main() {
    global x, y;

    int a = 0;
    int b = 5;

    int c = a + b;
    int d = a - b;
    int e = a * b;

    boolean f = a > b;
    boolean g = a == b;
    boolean h = a != b;

    // doStuff();
    // multiThread(a);
}

function int doStuff() {
    global z;

    boolean a = true;
    int[] a = [1,2,3];

    if (a) {
        z = false;
    }

    int c = 0;
    while (!z) {
        c += 4;
        if (c == 10) {
            z = true;
        }
    }

    return c;
}


function void multiThread(int a) {
    global y;

    int tid = fork(doStuff);

    join(tid);
}