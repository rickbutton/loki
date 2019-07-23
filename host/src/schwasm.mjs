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
const objTag = 0b001;

function stringToString(val, runtime) {
    return `\"${runtime.getString(val)}\"`;
}

function pairToString(val, runtime) {
    const car = runtime.car(val);
    const cdr = runtime.cdr(val);

    return `(${valueToString(car, runtime)} . ${valueToString(cdr, runtime)})`;
}

function objToString(val, runtime) {
    if (runtime.isPair(val)) {
        return pairToString(val, runtime);
    } else if (runtime.isClose(val)) {
        return "#<procedure>";
    } else if (runtime.isSlot(val)) {
        return `slot(${valueToString(runtime.unslot(val))})`;
    } else if (runtime.isString(val)) {
        return stringToString(val, runtime);
    }
}

function valueToString(expr, runtime) {
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
    } else if ((expr & objMask) == objTag) {
        return objToString(expr, runtime);
    } else {
        console.log("unknown expr");
        console.log(expr);
        console.log(expr.toString(2));
        throw expr;
    }
}

class Runtime {
    constructor(mod) {
        this.module = mod;
    }

    get exports() {
        return this.module.exports;
    }

    isSlot(val) { return this.exports.is_slot(val); }
    unslot(val) { return this.exports.unslot(val); }

    isPair(val) { return this.exports.is_pair(val); }
    car(pair) { return this.exports.car(pair); }
    cdr(pair) { return this.exports.cdr(pair); }

    isClose(val) { return this.exports.is_close(val); }
    isString(val) { return this.exports.is_string(val); }

    allocRodata() { return this.exports.alloc_rodata(); }
    getRodataOffset(id) { return this.exports.get_rodata_offset(id); }

    getStringLength(val) { return this.exports.get_string_length(val); }
    getStringOffset(val) { return this.exports.get_string_offset(val); }

    getString(val) {
        const length = this.getStringLength(val);
        const offset = this.getStringOffset(val);

        const buffer = this.module.exports.memory.buffer;
        const array = new Uint8Array(buffer, offset, length);
        return new TextDecoder("utf-8").decode(array);
    }
}

export class Schwasm {
    async init(runtimeBuffer) {
        const mod = await WebAssembly.compile(runtimeBuffer);
        this.runtime = new Runtime(await WebAssembly.instantiate(mod));
    }

    async load(buffer) {
        const mod = await WebAssembly.compile(buffer);

        const rodataId = this.runtime.allocRodata();
        const rodataOffset = this.runtime.getRodataOffset(rodataId);

        const instance = await WebAssembly.instantiate(mod, {
            env: {
                memory: this.runtime.exports.memory,
                "$$alloc_slot": this.runtime.exports.alloc_slot,
                "$$set_slot": this.runtime.exports.set_slot,
                "$$get_slot": this.runtime.exports.get_slot,
                "$$alloc_pair": this.runtime.exports.alloc_pair,
                "$$car": this.runtime.exports.car,
                "$$cdr": this.runtime.exports.cdr,
                "$$alloc_close": this.runtime.exports.alloc_close,
                "$$store_free": this.runtime.exports.store_free,
                "$$get_free": this.runtime.exports.get_free,
                "$$get_close_func_index": this.runtime.exports.get_close_func_index,
                "$$alloc_string": this.runtime.exports.alloc_string,

                "$$rodata-id": rodataId,
                "$$rodata-offset": rodataOffset,
            }
        });

        const main = instance.exports.main;
        const ret = main();
        return valueToString(ret, this.runtime);
    }
}
