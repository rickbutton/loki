const fixnumShift = 2;
const fixnumMask = 0b11;
const fixnumTag = 0b00;

const booleanTrue = 0b10011111;
const booleanFalse = 0b00011111;

const charShift = 8;
const charMask = 0b01111111;
const charTag =  0b00001111;

const nullTag = 0b00101111;

const objMask = 0b111;

const pairTag = 0b01;
const slotTag = 0b10;
const closeTag = 0b11;

function closeToString(ptr, runtime) {
    return "#<procedure>";
}

function pairToString(ptr, runtime) {

    const car = runtime.car(ptr);
    const cdr = runtime.cdr(ptr);

    return `(${schemeToString(car, runtime)} . ${schemeToString(cdr, runtime)})`;
}

function schemeToString(expr, runtime) {
    if ((expr & fixnumMask) === fixnumTag) {
        return String(expr >> fixnumShift);
    } else if ((expr & charMask) === charTag) {
        return String.fromCharCode(expr >> charShift);
    } else if (expr === booleanTrue) {
        return "true";
    } else if (expr === booleanFalse) {
        return "false";
    } else if (expr === nullTag) {
        return "()";
    } else if ((expr & objMask) === pairTag) {
        return pairToString(expr, runtime);
    } else if ((expr & objMask) === closeTag) {
        return closeToString(expr, runtime);
    } else {
        console.log("unknown expr");
        console.log(expr);
        throw expr;
    }
}

class Runtime {
    constructor(module) {
        this.module = module;
        this.memory = new Uint32Array(this.module.instance.exports.memory.buffer);
    }

    get exports() {
        return this.module.instance.exports;
    }

    car(pair) { return this.exports.car(pair); }
    cdr(pair) { return this.exports.cdr(pair); }
}

export class Schwasm {
    async init(runtimeBuffer) {
        this.runtime = new Runtime(await WebAssembly.instantiate(runtimeBuffer));
    }

    async load(buffer) {
        const mod = await WebAssembly.instantiate(buffer, {
            env: {
                memory: this.runtime.exports.memory,
                "$$alloc_slot": this.runtime.exports.alloc_slot,
                "$$unslot": this.runtime.exports.unslot,
                "$$alloc_pair": this.runtime.exports.alloc_pair,
                "$$car": this.runtime.exports.car,
                "$$cdr": this.runtime.exports.cdr,
                "$$alloc_close": this.runtime.exports.alloc_close,
                "$$store_free": this.runtime.exports.store_free,
                "$$get_free": this.runtime.exports.get_free,
                "$$get_close_func_index": this.runtime.exports.get_close_func_index,
            }
        });

        const main = mod.instance.exports.main;
        const ret = main();
        return schemeToString(ret, this.runtime);
    }
}