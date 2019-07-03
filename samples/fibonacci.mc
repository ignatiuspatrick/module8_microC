int a = 0;
int b = 1;

function int fib(int n) {
    int result = 0;

    if (n == 0) {
        result = 0;
    } else {
        int i = 2;
        int c = 0;
        while (i <= n) {
            c = a + b;
            a = b;
            b = c;
            i = i + 1;
        }
        result = b;
    }

    return result;
}

fib(7);