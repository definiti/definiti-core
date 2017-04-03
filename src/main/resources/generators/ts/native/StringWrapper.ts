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