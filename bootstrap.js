const wasmFile = process.argv[2];

const fixnumShift = 2;
const fixnumMask = 0b11;
const fixnumTag = 0b00;

const booleanTag = 0b0011111;
const booleanTrue = 0b10011111;
const booleanFalse = 0b00011111;

const charShift = 8;
const charMask = 0b11111111;
const charTag = 0b00001111;

const nullTag = 0b00101111;

var schemeToVal = function (expr) {
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
    else {
        console.log("unknown expr");
        console.log(expr);
        throw expr;
    }
}

var doExec = function (m) {
    var main = m.instance.exports.main;

    var out = main();
    console.log(schemeToVal(out));
}

const fs = require('fs');
const buf = fs.readFileSync(wasmFile);

WebAssembly.instantiate(new Uint8Array(buf))
    .then(m => doExec(m));