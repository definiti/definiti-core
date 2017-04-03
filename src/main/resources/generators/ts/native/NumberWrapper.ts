export class NumberWrapper {
    private inner: Number;

    constructor(inner: Number) {
        this.inner = inner;
    }

    toNumber() {
        return this.inner;
    }

    minus(numberWrapper: NumberWrapper): NumberWrapper {
        return new NumberWrapper(this.inner - numberWrapper.inner);
    }

    plus(numberWrapper: NumberWrapper): NumberWrapper {
        return new NumberWrapper(this.inner + numberWrapper.inner);
    }

    time(numberWrapper: NumberWrapper): NumberWrapper {
        return new NumberWrapper(this.inner * numberWrapper.inner);
    }

    divide(numberWrapper: NumberWrapper): NumberWrapper {
        return new NumberWrapper(this.inner / numberWrapper.inner);
    }

    modulo(numberWrapper: NumberWrapper): NumberWrapper {
        return new NumberWrapper(this.inner % numberWrapper.inner);
    }

    equals(numberWrapper: NumberWrapper): boolean {
        return this.inner === numberWrapper.inner;
    }

    notEquals(numberWrapper: NumberWrapper): boolean {
        return this.inner !== numberWrapper.inner;
    }

    upper(numberWrapper: NumberWrapper): boolean {
        return this.inner > numberWrapper.inner;
    }

    lower(numberWrapper: NumberWrapper): boolean {
        return this.inner < numberWrapper.inner;
    }

    upperOrEquals(numberWrapper: NumberWrapper): boolean {
        return this.inner >= numberWrapper.inner;
    }

    lowerOrEquals(numberWrapper: NumberWrapper): boolean {
        return this.inner <= numberWrapper.inner;
    }
}