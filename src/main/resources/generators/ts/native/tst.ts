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
export class StringWrapper{
    private inner: string;

    constructor(inner: string) {
        this.inner = inner || "";
    }

    toString() {
        return this.inner;
    }

    nonEmpty(): boolean {
        return this.inner.length > 0;
    }

    trim(): StringWrapper {
        return new StringWrapper(this.inner.trim());
    }

    startsWith(prefix: StringWrapper): boolean {
        return this.inner.substr(0, prefix.inner.length) === prefix.inner;
    }

    matches(regex: StringWrapper): boolean {
        return new RegExp(regex.inner).test(this.inner);
    }
}



export function verifyNonEmpty(string: StringWrapper): string|null {
    return verify("The string is empty // quoted comment", () => {
        return (string).nonEmpty();
    })
}



export function verifyNonBlank(string: StringWrapper): string|null {
    return verify("The string is blank /* quoted comment */", () => {
        return ((string).trim()).nonEmpty();
    })
}


/*
 Could be simplified, but it is for the "example"
 */
export function verifyPhoneNumber(string: StringWrapper): string|null {
    return verify("Please provide a phone number", () => {
        return  ((string).nonEmpty())
            ? ( ((string).startsWith(new StringWrapper("+33")))
                    ? ((string).matches(new StringWrapper("^\\+33\\d{9}$")))
                    : ((string).matches(new StringWrapper("^0\\d{9}$")))
            )
            : (false)
            ;
    })
}



export function verifyYearPeriod(period: $Period): string|null {
    return verify("The period must last one year", () => {
        return ((((period).end).timestamp).minus(((period).start).timestamp)) == ((new NumberWrapper(365)).time((new NumberWrapper(24)).time(new NumberWrapper(3600))));
    })
}



export function verifyStartJanuaryFirst(period: $Period): string|null {
    return verify("The period must start on january the first", () => {
        return ((((period).start).day).equals(new NumberWrapper(1))) && ((((period).start).month).equals(new NumberWrapper(1)));
    })
}


function verify(message: string, condition: () => Boolean): string|null {
    if (condition()) {
        return null;
    } else {
        return message;
    }
}



interface $Period {
    start: DateWrapper;
    end: DateWrapper;
}


export interface Period extends $Period {}

export function Period(start: DateWrapper, end: DateWrapper): string|Readonly<Period> {
    const __result = {start, end};
    const period = __result;
    let __errorOpt = null;
    const __verifications = [

        verify("end should be after start", () => {
            return (((period).end).upper((period).start)) || (((period).end).equals((period).start));
        })

    ];
    for (let __i = 0 ; __i < __verifications.length ; __i++) {
        if (__verifications[__i] !== null) {
            __errorOpt = __verifications[__i];
        }
    }

    if (__errorOpt) {
        return __errorOpt;
    } else {
        return Object.freeze(__result);
    }
}




export interface CivilYear extends $Period {}

export function CivilYear(start: DateWrapper, end: DateWrapper): string|Readonly<CivilYear> {
    const __result = {start, end};
    const period = __result;
    let __errorOpt = null;
    const __verifications = [

        verify("end should be after start", () => {
            return (((period).end).upper((period).start)) || (((period).end).equals((period).start));
        })
        , verifyYearPeriod(__result), verifyStartJanuaryFirst(__result)
    ];
    for (let __i = 0 ; __i < __verifications.length ; __i++) {
        if (__verifications[__i] !== null) {
            __errorOpt = __verifications[__i];
        }
    }

    if (__errorOpt) {
        return __errorOpt;
    } else {
        return Object.freeze(__result);
    }
}

const y = CivilYear(null, null);
if ((y as CivilYear).start) {

} else {

}