int currentMoney = 0;

currentMoney = 100;

fork (currentMoney) {
    int i = 0;
    lock currentMoney;
    while (i < 2) {
        currentMoney = currentMoney + 100;
        i = i + 1;
    }
    unlock currentMoney;

    fork (currentMoney) {
        int i = 0;
        lock currentMoney;
        while (i < 2) {
            currentMoney = currentMoney + 100;
            i = i + 1;
        }
        i = 0;
        while (i < 2) {
            currentMoney = currentMoney - 100;
            i = i + 1;
        }
        unlock currentMoney;
    } {

    }
} {
    int i = 0;
    lock currentMoney;
    while (i < 2) {
        currentMoney = currentMoney - 100;
        i = i + 1;
    }
    unlock currentMoney;
}

currentMoney = currentMoney;