const wasmFile = process.argv[2];

var typeMask = 0b11;
var typeShift = 2;

var fixnumTag = 0b00;
var booleanTag = 0b01;
var nullTag = 0b10;

var schemeToVal = function (expr) {
    if ((expr & typeMask) == fixnumTag) {
        return expr >> typeShift;
    } else if ((expr & typeMask) == booleanTag) {
        return (expr >> typeShift) == 1;
    } else if ((expr & typeMask) == nullTag) {
        return null;
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