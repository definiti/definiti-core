export class DateWrapper {
    private inner: Date;

    constructor(inner: Date) {
        this.inner = inner;
    }

    get timestamp() {
        return new NumberWrapper(this.inner.getTime());
    }

    get day() {
        return new NumberWrapper(this.inner.getDate());
    }

    get month() {
        return new NumberWrapper(this.inner.getMonth());
    }

    toDate() {
        return this.inner;
    }

    equals(date: DateWrapper): boolean {
        return this.timestamp == date.timestamp;
    }

    notEquals(date: DateWrapper): boolean {
        return this.timestamp != date.timestamp;
    }

    upper(date: DateWrapper): boolean {
        return this.timestamp > date.timestamp;
    }

    lower(date: DateWrapper): boolean {
        return this.timestamp < date.timestamp;
    }

    upperOrEquals(date: DateWrapper): boolean {
        return this.timestamp >= date.timestamp;
    }

    lowerOrEquals(date: DateWrapper): boolean {
        return this.timestamp <= date.timestamp;
    }
}