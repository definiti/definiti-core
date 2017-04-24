export class ListWrapper<A> {
    private inner: Array<A>;

    constructor(inner: Array<A>) {
        this.inner = inner;
    }

    nonEmpty(): boolean {
        return this.inner && this.inner.length > 0;
    }

    isEmpty(): boolean {
        return !this.nonEmpty();
    }

    get head(): A {
        return this.inner ? this.inner[0] : null;
    }

    randomElement(): A {
        // Not really random, it is for the example currently.
        return this.inner ? this.inner[0] : null;
    }

    forall(f: (A) => Boolean): Boolean {
        return this.inner.every(f);
    }

    exists(f: (A) => Boolean): Boolean {
        return this.inner.some(f);
    }

    foldLeft<B>(startValue: B, f: (B, A) => B): B {
        let acc = startValue;
        for (let i = 0 ; i < this.inner.length ; i++) {
            acc = f(acc, this.inner[i]);
        }
        return acc;
    }
}