int currentMoney = 0;

currentMoney = 100;

fork (currentMoney) {
    int i = 0;
    while (i < 2) {
        lock currentMoney;
        currentMoney = currentMoney + 100;
        unlock currentMoney;
        i = i + 1;
    }

    fork (currentMoney) {
        int i = 0;
        while (i < 2) {
            lock currentMoney;
            currentMoney = currentMoney + 100;
            unlock currentMoney;
            i = i + 1;
        }
    } {
        int i = 0;
        while (i < 2) {
            lock currentMoney;
            currentMoney = currentMoney - 100;
            unlock currentMoney;
            i = i + 1;
        }
    }
} {
    int i = 0;
    while (i < 2) {
        lock currentMoney;
        currentMoney = currentMoney - 100;
        unlock currentMoney;
        i = i + 1;
    }
}

currentMoney = currentMoney;