

const fixnumShift = 2;
const fixnumMask = 0b11;
const fixnumTag = 0b00;

const booleanTrue = 0b10011111;
const booleanFalse = 0b00011111;

const charShift = 8;
const charMask = 0b00011111;
const charTag =  0b00001111;

const nullTag = 0b00101111;

const objMask = 0b111;
const objShift = 3;

const pairTag = 0b01;
const slotTag = 0b10;

function schemePointerToJSPointer(ptr) {
    return (ptr >> 3) / 4;
}

function slotToString(ptr, memory) {
    const jsPtr = schemePointerToJSPointer(ptr);

    const val = memory[jsPtr];
    return schemeToVal(val, memory);
}

function pairToString(ptr, memory) {
    const jsPtr = schemePointerToJSPointer(ptr);

    const car = memory[jsPtr];
    const cdr = memory[jsPtr+1];

    const carValue = schemeToVal(car, memory);
    const cdrValue = schemeToVal(cdr, memory);

    return `(${carValue} . ${cdrValue})`;
}

function schemeToVal(expr, memory) {
    if ((expr & fixnumMask) === fixnumTag)
        return expr >> fixnumShift;
    else if ((expr & charMask) === charTag)
        return String.fromCharCode(expr >> charShift);
    else if (expr === booleanTrue)
        return true;
    else if (expr === booleanFalse)
        return false;
    else if (expr === nullTag)
        return null;
    else if ((expr & objMask) === pairTag)
        return pairToString(expr, memory);
    else if ((expr & objMask) === slotTag) {
        return slotToString(expr, memory);
    } else {
        console.log("unknown expr");
        console.log(expr);
        throw expr;
    }
}

function doExec(mod) {
    const buffer = mod.instance.exports.memory.buffer;
    const memory = new Uint32Array(buffer);

    const main = mod.instance.exports.main;
    var out = main();
    console.log(out);
    console.log(schemeToVal(out, memory));
}

async function getModuleBrowser() {
    const response = await fetch("bin/a.wasm");
    const buffer = await response.arrayBuffer();
    const m = await WebAssembly.instantiate(buffer);
    doExec(m);
}

async function getModuleNode() {
    const fs = require('fs');
    const wasmFile = process.argv[2];
    const buf = new Uint8Array(fs.readFileSync(wasmFile));
    const m = await WebAssembly.instantiate(buf);
    doExec(m);
}

const wasm = typeof window === "undefined" ? getModuleNode() : getModuleBrowser();

wasm.catch(error => {
    console.error(error);
});