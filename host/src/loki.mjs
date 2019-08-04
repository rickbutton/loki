function error(msg) {
    throw new Error(msg);
}

const Void = {};

class Slot {
    constructor() {
        this.value = Void;
    }
}
class Pair {
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
}
class Closure {
    constructor(index, nbounds, nfrees) {
        this.index = index;
        this.nbounds = nbounds;
        this.nfrees = nfrees;
        this.frees = [];
    }
}

function isNumber(v) { return typeof v === "number" && ((v & 1) === 0); }
function isChar(v) { return typeof v === "number" && ((v & 1) === 1); }
function isBoolean(v) { return typeof v === "boolean"; }

function isSlot(v) { return v instanceof Slot; }
function isPair(v) { return v instanceof Pair; }
function isClosure(v) { return v instanceof Closure; }
function isString(v) { return typeof v === "string"; }

function assertPrimArgIsType(prim, value, typePred) {
    const values = Array.isArray(value) ? value : [value];
    for (const v of values) {
        if (!typePred(v)) {
            throw new Error(`invalid argument type to primitive ${prim}`);
        }
    }
}

function schemeValueToString(value) {
    if (isNumber(value)) {
        return String(value >> 1);
    } else if (isChar(value)) {
        return `#\\${String.fromCharCode(value >> 1)}`;
    } else if (isBoolean(value)) {
        return value ? "#t" : "#f";
    } else if (isString(value)) {
        return `"${value}"`;
    } else if (isPair(value)) {
        const car = schemeValueToString(value.car);
        const cdr = schemeValueToString(value.cdr);
        return `(${car} . ${cdr})`;
    } else if (value === null) {
        return "()";
    } else if (value === Void) {
        return "<#void>";
    } else if (isClosure(value)) {
        return `<#closure(${value.index})>`;
    } else {
        throw new Error(`unknown value ${value}`);
    }
}

function getPrimitives(memory) {
    return {
        "$$prim$make-number": (n) => n << 1,
        "$$prim$add": (a, b) => {
            assertPrimArgIsType("$$prim$add", [a, b], isNumber);
            return a + b;
        },
        "$$prim$sub": (a, b) => {
            assertPrimArgIsType("$$prim$sub", [a, b], isNumber);
            return a - b;
        },
        "$$prim$le_s": (a, b) => {
            assertPrimArgIsType("$$prim$le_s", [a, b], isNumber);
            return a <= b;
        },
        "$$prim$test": (v) => {
            return v === false ? 0 : 1;
        },

        "$$prim$make-char": (n) => (n << 1) | 1,

        "$$prim$make-slot": () => new Slot(),
        "$$prim$set-slot": (v, s) => {
            assertPrimArgIsType("$$prim$set-slot", s, isSlot);
            s.value = v;
        },
        "$$prim$get-slot": (s) => {
            if (isSlot(s)) {
                return s.value;
            } else {
                return s;
            }
        },
        "$$prim$cons": (car, cdr) => new Pair(car, cdr),
        "$$prim$car": (pair) => {
            assertPrimArgIsType("$$prim$car", pair, isPair);
            return pair.car;
        },
        "$$prim$cdr": (pair) => {
            assertPrimArgIsType("$$prim$car", pair, isPair);
            return pair.cdr;
        },
        "$$prim$make-closure": (findex, nbounds, nfrees) => {
            return new Closure(findex, nbounds, nfrees);
        },
        "$$prim$set-free": (c, i, v) => {
            assertPrimArgIsType("$$prim$set-free", c, isClosure);
            c.frees[i] = v;
            return c;
        },
        "$$prim$get-free": (c, i) => {
            assertPrimArgIsType("$$prim$get-free", c, isClosure);
            return c.frees[i];
        },
        "$$prim$get-closure-findex": (c, nbounds) => {
            assertPrimArgIsType("$$prim$get-closure-findex", c, isClosure);
            if (c.nbounds !== nbounds) {
                error(`invalid application, expected ${c.nbounds} arguments, but received ${nbounds}`);
            }
            return c.index;
        },
        "$$prim$make-string": (offset, length) => {
            const view = new Uint8Array(memory.buffer, offset, length);
            return new TextDecoder().decode(view);
        },
        "$$prim$concat-string": (a, b) => {
            assertPrimArgIsType("$$prim$concat-string", [a, b], isString);
            return a + b;
        },
        "$$iv$true": true,
        "$$iv$false": false,
        "$$iv$null": null,
        "$$iv$void": Void,
    };
}

export class Loki {
    async load(buffer) {
        const mod = await WebAssembly.compile(buffer);

        const memory = new WebAssembly.Memory({ initial: 1 });
        const primitives = getPrimitives(memory);
        const instance = await WebAssembly.instantiate(mod, {
            env: Object.assign({ memory }, primitives),
        });

        const init = instance.exports.init;
        const main = instance.exports.main;

        init();
        const ret = main();
        return schemeValueToString(ret, this.runtime);
    }
}
